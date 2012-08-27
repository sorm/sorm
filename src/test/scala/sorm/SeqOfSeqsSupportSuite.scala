package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import core._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import extensions.Extensions._

import samples._
import Sorm._

@RunWith(classOf[JUnitRunner])
class SeqOfSeqsSupportSuite extends FunSuite with ShouldMatchers {

  import SeqOfSeqsSupportSuite._

  val db = TestingInstance.h2( Entity[A]() )
  db.save(A( Seq() ))
  db.save(A( Seq( Seq(2, 3), Seq(), Seq(7) ) ))
  db.save(A( Seq( Seq() ) ))
  db.save(A( Seq( Seq(78) ) ))
  db.save(A( Seq() ))


  test("Empty Seq matches empty Seq and not Seq of empty Seq"){
    db.query[A]
      .filterEqual("a", Seq())
      .fetchAll().view.map{_.id}.toSet
      .should(
        contain (1l) and
        contain (5l) and
        not contain (3l)
      )
  }
  test("An empty item Seq does not match inexistent one"){
    db.query[A]
      .filterEqual("a.item", Seq())
      .fetchAll().view.map{_.id}.toSet 
      .should(
        not contain (1l) and
        not contain (5l)
      )
  }
  test("A partially matching seq with container seq containing other seq of same size"){
    db.query[A]
      .filterEqual("a.item", Seq(2))
      .fetchAll().view.map{_.id}.toSet
      .should( not contain (2l) )
  }
}
object SeqOfSeqsSupportSuite {
  case class A ( a : Seq[Seq[Int]] )

}