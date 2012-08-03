package vorm

import vorm._
import structure._
import query._
import mapping._
import vorm.{sql => Sql}
import ddl._
import extensions._

package object select {

  sealed case class Statement
    ( template : String,
      data : Seq[Any],
      resultMappings : Seq[(TableMapping, Column)] )
 
  implicit class QueryExtensions
    ( q : Query )
    {
      lazy val resultSetSelect
        = MappingSelect(q.mapping)
            .resultSet
            .withOrders(q.order)
 
      lazy val primaryKeySelect
        = MappingSelect( q.mapping, 
                         limit = q.limit,
                         offset = if( q.offset == 0 ) None 
                                  else Some(q.offset) )
            .foldFrom(q.where){_ withWhere _}
            .primaryKey
 
      lazy val sql
        = resultSetSelect.sql narrow primaryKeySelect.sql
 
      lazy val resultMappings
        : Seq[(TableMapping, Column)]
        = resultSetSelect.resultMappings
 
      def statement
        = Statement( sql.rendering, sql.data, resultMappings )
 
    }

}