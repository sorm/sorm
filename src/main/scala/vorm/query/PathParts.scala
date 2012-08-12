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

object PathParts {

  trait Part

  case class Property ( name : String ) extends Part
  case class Key ( name : String ) extends Part
  case class Index ( index : Int ) extends Part

  def apply
    ( p : String )
    : Stream[Part]
    = ???

}
