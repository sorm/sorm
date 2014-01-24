package sorm.relational.model

case class Table
  ( name        : String,
    columns     : Seq[Column],
    primaryKey  : Seq[String],
    uniqueKeys  : Set[Seq[String]] = Set.empty,
    indexes     : Set[Seq[String]] = Set.empty,
    foreignKeys : Set[ForeignKey] = Set.empty )

case class ForeignKey
  ( table     : String,
    bindings  : Seq[(String, String)],
    onDelete  : ReferenceMode = ReferenceMode.NoAction,
    onUpdate  : ReferenceMode = ReferenceMode.NoAction )

sealed trait ReferenceMode
object ReferenceMode {
  case object Restrict    extends ReferenceMode
  case object Cascade     extends ReferenceMode
  case object NoAction    extends ReferenceMode
  case object SetNull     extends ReferenceMode
  case object SetDefault  extends ReferenceMode
}

case class Column
  ( name          : String,
    t             : ColumnType,
    autoIncrement : Boolean = false,
    nullable      : Boolean = false )

sealed trait ColumnType
object ColumnType {
  case class Enum( values : Seq[String] ) extends ColumnType
  case object Time extends ColumnType
  case object Date extends ColumnType
  case object TimeStamp extends ColumnType
  case object Integer extends ColumnType
  case object VarChar extends ColumnType
  case object Double extends ColumnType
  case object Float extends ColumnType
  case object Text extends ColumnType
  case object BigInt extends ColumnType
  case object Boolean extends ColumnType
  case object Decimal extends ColumnType
  case object SmallInt extends ColumnType
  case object TinyInt extends ColumnType

  def jdbcType( t : ColumnType ) = {
    import java.sql.Types._
    t match {
      case Enum(_)   => VARCHAR
      case Time      => TIME
      case Date      => DATE
      case TimeStamp => TIMESTAMP
      case Integer   => INTEGER
      case VarChar   => VARCHAR
      case Double    => DOUBLE
      case Float     => FLOAT
      case Text      => CLOB
      case BigInt    => BIGINT
      case Boolean   => BOOLEAN
      case Decimal   => DECIMAL
      case SmallInt  => SMALLINT
      case TinyInt   => TINYINT
    }
  }
}
