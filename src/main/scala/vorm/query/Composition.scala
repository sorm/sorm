package vorm.query

import vorm.reflection._
import vorm.structure._
import vorm.structure.mapping._
import vorm.persisted._
import vorm.extensions._

import Query._
import Operator._
import vorm.abstractSql.{AbstractSql => AS}
import vorm.abstractSql.Composition._

object Composition {

  def rootKeyStatement
    ( query : Query )
    : AS.Statement
    = ???

  def rootKeyStatement
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
          val matching
            = v.view
                .zipWithIndex
                .map{ case (v, i) =>
                  intersection(
                    rootKeyStatement(Filter(Equals, m.index, i)),
                    rootKeyStatement(Filter(Equals, m.item, v))
                  )
                }
                .reduceOption{union}
                .map{
                  select(_).copy(
                    havingCount
                      = Some(AS.HavingCount(m.abstractSqlTable, v.size))
                  )
                }
          val size
            = m.root.abstractSqlPrimaryKeySelect
                .copy(
                  havingCount
                    = Some(AS.HavingCount(m.abstractSqlTable, v.size))
                )
          matching.foldLeft(size : AS.Statement){AS.Intersection}
      }

}