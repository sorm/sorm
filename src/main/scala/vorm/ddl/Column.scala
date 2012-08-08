package vorm.ddl

import vorm._
import extensions._

sealed case class Column
  ( name : String,
    t : Column.Type,
    autoIncrement : Boolean = false,
    nullable : Boolean = false )
  {
    def ddl
      = quote(name) + " " + t.ddl + 
        ( if( nullable ) "" else " NOT NULL" ) +
        ( if( autoIncrement ) " AUTO_INCREMENT" else "" )
  }

object Column {

  sealed trait Type {
    def ddl : String
    def jdbcType : Int
  }

  object Type {

    import java.sql.Types

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
        val ddl = "TEXT"
        val jdbcType = Types.LONGVARCHAR
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
      
  }

}