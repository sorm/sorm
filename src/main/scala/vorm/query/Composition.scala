package vorm.query

import vorm._
import reflection._
import structure._
import mapping._
import persisted._
import abstractSql._
import extensions._

import Query._
import Operator._
import abstractSql.Composition._

object Composition {

  def rootKeyStatement
    ( query : Query )
    : AbstractSql.Statement
    = ???

  def rootKeyStatement
    ( where : Where )
    : AbstractSql.Statement
    = where match {
        case Filter(Equals, m : SeqMapping, v : Seq[_]) =>
          AbstractSql.Intersection(
            v.view
              .zipWithIndex
              .map{ case (v, i) =>
                rootKeyStatement(Filter(Equals, m.index, i)) intersect
                rootKeyStatement(Filter(Equals, m.item, v))
              }
              .reduceOption{ _ union _ }
              .map{ _.toSelect }
              .getOrElse( m.root.abstractSqlPrimaryKeySelect )
              .copy(
                havingCount
                  = Some(AbstractSql.HavingCount(m.abstractSqlTable, v.size))
              ),
            m.root.abstractSqlPrimaryKeySelect
              .copy(
                havingCount
                  = Some(AbstractSql.HavingCount(m.abstractSqlTable, v.size))
              )
          )
      }

}