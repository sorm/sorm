package sorm.ddl

import sorm._
import sext._

sealed case class Column
  ( name : String,
    t : Column.Type,
    autoIncrement : Boolean = false,
    nullable : Boolean = false )
  {
    def ddl
      = quote(name) + " " + t.ddl + 
        ( if( nullable ) " NULL" else " NOT NULL" ) +
        ( if( autoIncrement ) " AUTO_INCREMENT" else "" )
    override def toString
      = name
  }

object Column {

  sealed trait Type {
    def ddl : String
    def jdbcType : Int
  }

  object Type {

    import java.sql.Types

    case class Enum 
      ( values : Seq[String] )
      extends Type
      {
        lazy val ddl 
          = "ENUM(" + values.map("'" + _ + "'").mkString(", ") + ")"
        val jdbcType = Types.VARCHAR
      }

    case object Time
      extends Type
      {
        val ddl = "TIME"
        val jdbcType = Types.TIME
      }
    case object Date
      extends Type
      {
        val ddl = "DATE"
        val jdbcType = Types.DATE
      }
    case object TimeStamp
      extends Type
      {
        val ddl = "TIMESTAMP"
        val jdbcType = Types.TIMESTAMP
      }
    case object Integer  
      extends Type
      {
        val ddl = "INTEGER"
        val jdbcType = Types.INTEGER
      }
    case object VarChar  
      extends Type
      {
        val ddl = "VARCHAR(255)"
        val jdbcType = Types.VARCHAR
      }
    case object Double   
      extends Type
      {
        val ddl = "DOUBLE"
        val jdbcType = Types.DOUBLE
      }
    case object Float   
      extends Type
      {
        val ddl = "FLOAT"
        val jdbcType = Types.FLOAT
      }
    case object Text    
      extends Type
      {
        val ddl = "MEDIUMTEXT"
        val jdbcType = Types.CLOB
      }
    case object BigInt   
      extends Type
      {
        val ddl = "BIGINT"
        val jdbcType = Types.BIGINT
      }
    case object Boolean  
      extends Type
      {
        val ddl = "TINYINT(1)"
        val jdbcType = Types.BOOLEAN
      }
    case object Decimal  
      extends Type
      {
        val ddl = "DECIMAL"
        val jdbcType = Types.DECIMAL
      }
    case object SmallInt 
      extends Type
      {
        val ddl = "SMALLINT"
        val jdbcType = Types.SMALLINT
      }
    case object TinyInt
      extends Type
      {
        val ddl = "TINYINT"
        val jdbcType = Types.TINYINT
      }
      
  }

}