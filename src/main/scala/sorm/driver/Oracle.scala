package sorm.driver

import java.sql.ResultSet

import embrace._
import org.joda.time.DateTime
import sext._
import sorm.core.SormException
import sorm.ddl._
import sorm.driver.Oracle.Constants._
import sorm.driver.Oracle.Numbers
import sorm.jdbc.{Statement, _}
import sorm.sql.Sql
import sorm.sql.Sql.{Insert, Join, JoinKind, Select}

import scala.util.Try

object Oracle {
  object Constants {
    val SequenceSuffix = "s"
    val TriggerSuffix = "t"
    val IndexSuffix = "i"
    val IdColumnName = "id"
  }

  sealed abstract class Number[T](val convert: (ResultSet, Int) => T, val precision: Int, val scale: Int = 0) {
    override def toString: String = s"NUMBER($precision, $scale)"
  }

  object Numbers {
    object Boolean extends Number({case (rs, i) => rs.getInt(i) != 0},  1)
    object TinyInt extends Number({case (rs, i) => rs.getByte(i)},  4)
    object SmallInt extends Number({case (rs, i) => rs.getShort(i)},  6)
    object Integer extends Number({case (rs, i) => rs.getInt(i)},  11)
    object BigInt extends Number({case (rs, i) => rs.getLong(i)},  38)
    object Decimal extends Number({case (rs, i) => new BigDecimal(rs.getBigDecimal(i))},  38, 19)
    object Double extends Number({case (rs, i) => rs.getDouble(i)},  38, 18)

    private lazy val all = List(BigInt, Integer, Decimal, Double, SmallInt, TinyInt, Boolean)

    def convert(rs: ResultSet, i: Int): Option[Any] = {
      val meta = rs.getMetaData
      val (precision, scale) = (meta.getPrecision(i), meta.getScale(i))
      all.find{n => n.precision == precision && n.scale == scale}.map(_.convert(rs, i))
    }
  }
}

class Oracle(protected val connection : JdbcConnection)
  extends DriverConnection
  with StdConnection
  with StdTransaction
  with StdAbstractSqlToSql
  with StdQuote
  with StdSqlRendering
  with StdStatement
  with StdQuery
  with StdModify
  with StdCreateTable
  with StdListTables
  with StdDropTables
  with StdDropAllTables
  with StdNow
{


  override protected def quote ( x : String ) = "\"" + x + "\""
  override def createTable(table: Table) = {

    super.createTable(table)

    table.columns.filter(_.autoIncrement).foreach{ col => dropAutoIncrement(table.name, col.name)}

    val triggers = table.columns.filter(_.autoIncrement).flatMap{ col => createAutoIncrement(table.name, col.name)}
    val indexes = {
      val numbered = table.indexes.map { names =>
        names.map{ name => table.columns.zipWithIndex.find(_._1.name == name).get._2}
      }
      table.indexes.zip(numbered).map(createIndexDdl(table.name, _))
    }

    (triggers ++ indexes).foreach { stmt =>
      stmt $ (Statement(_)) $ connection.executeUpdate
    }

  }

  protected def createIndexDdl( table: String, columns: (Seq[String], Seq[Int]) ): String
  = "CREATE INDEX " + quote(s"${table}_$IndexSuffix" + columns._2.mkString("_")) + " ON " + quote(table) + " (" + columns._1.view.map(quote).mkString(", ") + ")"

  override def dropTable ( table : String ) {
    table $ ("DROP TABLE " + quote(_)) $ (Statement(_)) $ connection.executeUpdate
    dropAutoIncrement(table, IdColumnName)
  }

  override def dropAllTables() {
    def tryToDrop
    ( table : String )
    {
      try {
        dropTable(table)
      } catch {
        case e : Throwable =>
      }
    }

    var lastTables = List[String]()
    var tables = listTables()
    while( tables != lastTables ) {
      tables foreach tryToDrop
      lastTables = tables
      tables = listTables()
    }

    if( lastTables.nonEmpty ) {
      throw new SormException("Couldn't drop all tables")
    }

  }


  protected def dropAutoIncrement(table: String, column: String): Unit = {
    val seq = "DROP SEQUENCE "+ quote(s"${table}_${column}_$SequenceSuffix")
    //val trg = "DROP TRIGGER "+ quote(s"${table}_${column}_$TriggerSuffix")

    List(seq).foreach { stmt =>
      Try(stmt $ (Statement(_)) $ connection.executeUpdate)
    }
  }

  protected def createAutoIncrement(tableNam: String, columnNam: String): Seq[String] = {
    val table = quote(tableNam)
    val column = quote(columnNam)

    val prefix = s"${tableNam}_$columnNam"

    val sequence = quote(s"${prefix}_$SequenceSuffix")
    val seqSql = "CREATE SEQUENCE " + sequence

    val trigger = quote(s"${prefix}_$TriggerSuffix")

    val trgSql = s"CREATE TRIGGER $trigger BEFORE INSERT ON $table FOR EACH ROW WHEN (new.$column IS NULL) BEGIN SELECT $sequence.nextval INTO :new.$column FROM DUAL; END;"
    seqSql :: trgSql:: Nil
  }

  override protected def indexDdl(columns: Seq[String]) = ""

  override protected def showTablesSql
    = "SELECT table_name FROM user_tables"

  override protected def columnDdl(c: Column)
    = if ( !c.autoIncrement ) super.columnDdl(c)
      else quote(c.name) + s" ${Numbers.BigInt} " + c.nullable.option("NULL").getOrElse("NOT NULL")

  override protected def columnTypeDdl ( t : ColumnType )
    = {
      import ColumnType._
      t match {
        case Boolean => Numbers.Boolean.toString
        case TinyInt => Numbers.TinyInt.toString
        case SmallInt => Numbers.SmallInt.toString
        case Integer => Numbers.Integer.toString
        case BigInt => Numbers.BigInt.toString
        case Decimal => Numbers.Decimal.toString
        case Double => Numbers.Double.toString
        case Text => "VARCHAR2(4000)"
        case _ => super.columnTypeDdl(t)
      }
    }
  override def insertAndGetGeneratedKeys(table: String, values: Iterable[(String, Any)]) : Seq[Any] = {
    val insert = values.toStream.unzip $$ (Insert(table, _, _)) $ statement
    val sequenceName = quote(s"${table}_${IdColumnName}_$SequenceSuffix")
    connection.executeUpdateAndGetGeneratedKeysOracle(insert, sequenceName).head.take(1)
  }


  override def now(): DateTime = connection
    .executeQuery(Statement("SELECT CURRENT_TIMESTAMP FROM DUAL"))()
    .head.head
    .asInstanceOf[DateTime]


  override protected def foreingKeyDdl ( fk : ForeignKey )
  = {
    val ForeignKey(table, bindings, onDelete, onUpdate) = fk
    "FOREIGN KEY\n" +
      ( "( " + bindings.view.unzip._1.map(quote).mkString(", ") + " )\n" +
        "REFERENCES " + quote(table) + "\n" +
        "( " + bindings.view.unzip._2.map(quote).mkString(", ") + " )\n" +
        "ON DELETE " + referenceOptionDdl(onDelete)
        ) .indent(2)
  }


  override protected def template(sql: Sql) = {
    import sorm.sql.Sql.{From, Table}

    sql match {
      case From(what, as) =>
        "FROM\n" +
          ( ( what match {
            case Table(name) ⇒ quote(name)
            case s : Sql ⇒ "(\n" + template(s).indent(2) + "\n)"
          } ) +
            as.map{ "\n" + quote(_) }.getOrElse("")
            )
            .indent(2)
      case Select(what, from, join, where, groupBy, having, orderBy, limit, offset, distinct) =>
        val select =
        "SELECT" + distinct.option(" DISTINCT").mkString + "\n" +
          ( what.view.map{template(_)}.mkString(", ") +
            "\n" + template(from) +
            join
              .view
              .map{template(_)}
              .mkString("\n")
              .satisfying{ ! _.isEmpty }
              .map{"\n" + _}
              .getOrElse("") +
            where
              .map{template(_)}
              .map{ "\nWHERE " + _.indent("WHERE ".length).trim }
              .getOrElse("") +
            groupBy
              .view
              .map{template(_)}
              .mkString(", ")
              .satisfying{ ! _.isEmpty }
              .map{ "\nGROUP BY " + _ }
              .getOrElse("") +
            having
              .map{template(_)}
              .map{ "\nHAVING " + _.indent("HAVING ".length).trim }
              .getOrElse("") +
            orderBy
              .map{template(_)}
              .mkString(", ")
              .satisfying{ ! _.isEmpty }
              .map{ "\nORDER BY " + _ }
              .getOrElse("") +
            offset
              .map{ "\nOFFSET " + _ }
              .getOrElse("") )
            .indent(2)
        limit.map{max => s"SELECT * FROM ($select) WHERE ROWNUM <= $max"}.getOrElse(select)

      case Join(what, as, on, kind) =>
        ( kind match {
          case JoinKind.Left ⇒ "LEFT JOIN "
          case JoinKind.Right ⇒ "RIGHT JOIN "
          case JoinKind.Inner ⇒ "INNER JOIN "
        } ) + "\n" +
          ( ( what match {
            case Table(name) ⇒ quote(name)
            case s : Sql ⇒ "( " + template(s).indent(2).trim + " )"
          } ) +
            as.map{ "\n" + quote(_) }
              .getOrElse("") +
            on.map{ case (l, r) ⇒ template(l) + " = " + template(r) }
              .mkString(" AND\n")
              .satisfying{ ! _.isEmpty }
              .map{ "\nON " + _.indent("ON ".length).trim }
              .getOrElse("")
            )
            .indent(2)

      case _ => super.template(sql)
    }
  }
}