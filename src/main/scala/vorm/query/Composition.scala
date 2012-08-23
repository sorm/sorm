package vorm.query

import vorm._
import reflection._
import structure._
import mapping._
import persisted._
import sql._
import extensions._

import Query._
import Operator._
import AbstractSql._

object Composition {

  def rootKeyStatement
    ( query : Query )
    : Statement
    = ???

  def rootKeyStatement
    ( where : Where )
    : Statement
    = where match {
        case Filter(Equals, mapping : SeqMapping, value : Seq[_]) =>
          value
            .view
            .zipWithIndex
            .map{ case (v, i) =>
              intersect(
                rootKeyStatement(m.index, i),
                rootKeyStatement(m.item, v)
              )
            }
      }

  def intersect
    ( left : Statement, right : Statement )
    : Statement
    = ???

}