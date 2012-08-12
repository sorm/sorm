package vorm.query

import vorm._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import query._
import select._
import resultSet._
import extensions._

import Query._

import collection.immutable.Queue

object Path {

  trait Part

  case class DotName ( name : String ) extends Part
  case class RoundBracesName ( name : String ) extends Part
  case class RoundBracesIndex ( index : Int ) extends Part

  def apply
    ( p : String )
    : Stream[Part]
    = ???

}
