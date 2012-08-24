package vorm.query

import vorm.reflection._
import vorm.structure._
import vorm.structure.mapping._
import vorm.persisted._
import vorm.extensions._

import vorm.abstractSql.{AbstractSql => AS}
import vorm.abstractSql.Compositing._
import Query._
import Operator._
import AbstractSqlCombinators._

object AbstractSqlComposition {

  def resultSetSelect
    ( query : Query )
    : AS.Statement
    = ( ( query.mapping.abstractSqlResultSetSelect : AS.Statement ) /:
        ( orderAndLimitSelect(query) ++
          query.where.map{filtersStatement} reduceOption intersection )
      ) {AS.Intersection}

  def orderAndLimitSelect
    ( query : Query )
    : Option[AS.Statement]
    = query
        .satisfying{q => q.order.nonEmpty || q.limit.nonEmpty || q.offset != 0}
        .map{ q =>
          q.mapping.root.abstractSqlPrimaryKeySelect
            .copy(
              order
                = q.order.map{ case Order(m, r) =>
                    AS.Order(
                      m.containerTableMapping.get.abstractSqlTable,
                      m.columnName,
                      r
                    )
                  },
              limit
                = q.limit,
              offset
                = q.offset
            )
        }

  def filtersStatement
    ( where : Where )
    : AS.Statement
    = where match {
        case Filter(Equals, m, v) =>
          equaling(m, v)
        case Filter(NotEquals, m, v) =>
          notEqualing(m, v)
      }


}