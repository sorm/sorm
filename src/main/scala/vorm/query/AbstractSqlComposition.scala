package vorm.query

import vorm.extensions._

import vorm.abstractSql.{AbstractSql => AS}
import vorm.abstractSql.Combinators._
import Query._
import Operator._

object AbstractSqlComposition {

  def resultSetSelect
    ( query : Query )
    : AS.Statement
    = query.mapping.abstractSqlResultSetSelect &&!
      ( orderAndLimitSelect(query) ++
        query.where.map{filtersStatement} reduceOption ( _ & _ ) )

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
        case And(l, r) =>
          filtersStatement(l) & filtersStatement(r)
        case Or(l, r) =>
          filtersStatement(l) | filtersStatement(r)
        case Filter(Equals, m, v) =>
          equaling(m, v)
        case Filter(NotEquals, m, v) =>
          notEqualing(m, v)
      }


}