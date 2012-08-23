package vorm.query

import vorm._
import reflection._
import structure._
import mapping._
import jdbc._
import sql._
import extensions._

import Composition._
import Query._
import Operator._

object Rendering {

  def idSql
    ( where : Where )
    : Sql.Statement
    = where match {
        case Filter(Equals, mapping : SeqMapping, value : Seq[_]) =>
          mapping.skeletonSelect
      }

}