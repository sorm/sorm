package sorm

import sorm._
import structure._
import mapping._
import jdbc._
import sql._
import save._
import persisted._
import extensions.Extensions._

/**
 * This implementation is based on strict rules about how statements are resolved to whether be Update or Insert. These are:
 * 1. All entities must have an autoincremented id property
 * 2. Whether this property is defined determines which method to use
 * 3. This may result in updates being called on inexisting rows - these situations should be considered caused by improper use and as such should result in a runtime exception being thrown
 * 4. All rows of collection tables are deleted before update and thus should result in no such conflicts
 */
package object save {

  // implicit class MappingResultSetParsingAdapter
  //   ( m : Mapping )
  //   {
  //     def propertyValues 
  //       ( rs : ResultSet, 
  //         columns : Seq[Column] ) 
  //       : Seq[Map[String, Any]]
  //   }
  // class MappedResultSet
  //   ( val resultSet : ResultSet,
  //     val columns : Seq[(Column, TableMapping)] )
  
  def rowValuesForContainerTable
    ( v : Any,
      m : Mapping )
    : Map[String, JdbcValue]
    = (m, v) match {
        case (m : CollectionMapping, _) =>
          Map.empty
        case (m : EntityMapping, v : Persisted) =>
          Map( m.columnName + "$id" -> JdbcValue(v.id, java.sql.Types.BIGINT) )
        case (m : TupleMapping, v : Product) =>
          (v.productIterator.toStream zip m.items)
            .flatMap{ case (v, m) => rowValuesForContainerTable(v, m) }
            .toMap
        case (m : OptionMapping, Some(v)) =>
          rowValuesForContainerTable(v, m.item)
        case (m : OptionMapping, None) =>
          m.columns
            .view
            .map{ c => c.name -> JdbcValue(null, java.sql.Types.NULL) }
            .toMap
        case (m : ValueMapping, v) =>
          Map( m.columnName -> JdbcValue(v, m.column.t.jdbcType) )

      }
      
  def quote
    ( s : String )
    = s

}