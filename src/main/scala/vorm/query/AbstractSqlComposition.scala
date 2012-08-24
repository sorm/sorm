package vorm.query

import vorm.reflection._
import vorm.structure._
import vorm.structure.mapping._
import vorm.persisted._
import vorm.extensions._

import Query._
import Operator._
import vorm.abstractSql.{AbstractSql => AS}
import vorm.abstractSql.Compositing._

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
        case Filter(Equals, m : ValueMapping, v) =>
          m.root.abstractSqlPrimaryKeySelect
            .copy(
              condition
                = Some(
                    AS.Comparison(
                      m.containerTableMapping.get.abstractSqlTable,
                      m.columnName,
                      AS.Equal,
                      v
                    )
                  )
            )
        case Filter(Equals, m : SeqMapping, v : Seq[_]) =>
          def intersects
            = v.view
                .zipWithIndex
                .map{ case (v, i) =>
                  intersection(
                    filtersStatement(Filter(Equals, m.index, i)),
                    filtersStatement(Filter(Equals, m.item, v))
                  )
                }
                .reduceOption{union}
                .map{
                  //  a probable pitfall, because it shouldn't support nested seqs:
                  // select(_)
                  // - so we make it fail in those situations for now:
                  _.asInstanceOf[AS.Select]
                    .copy(
                      havingCount
                        = Some(
                            AS.HavingCount(
                              m.abstractSqlTable,
                              m.index.columnName,
                              AS.Equal,
                              v.size
                            )
                          )
                    )
                }
          def hasSameSize
            = m.root.abstractSqlPrimaryKeySelect
                .copy(
                  havingCount
                    = Some(
                        AS.HavingCount(
                          m.abstractSqlTable,
                          m.index.columnName,
                          AS.Equal,
                          v.size
                        )
                      )
                )
          intersects.foldLeft(hasSameSize : AS.Statement){AS.Intersection}
        case Filter(NotEquals, m : ValueMapping, v) =>
          m.root.abstractSqlPrimaryKeySelect
            .copy(
              condition
                = Some(
                    AS.Comparison(
                      m.containerTableMapping.get.abstractSqlTable,
                      m.columnName,
                      AS.NotEqual,
                      v
                    )
                  )
            )
        case Filter(NotEquals, m : SeqMapping, v : Seq[_]) =>
          def doesntIntersect
            = v.view.zipWithIndex
                .map{ case (v, i) =>
                  intersection(
                    filtersStatement(Filter(Equals, m.index, i)),
                    filtersStatement(Filter(NotEquals, m.item, v))
                  )
                }
                .reduceOption{union}
          def sizesDontMatch
            = m.root.abstractSqlPrimaryKeySelect
                .copy(
                  havingCount
                    = Some(
                        AS.HavingCount(
                          m.abstractSqlTable,
                          m.index.columnName,
                          AS.NotEqual,
                          v.size
                        )
                      )
                )
          ( (sizesDontMatch : AS.Statement) /: doesntIntersect ){ AS.Union }

      }

}