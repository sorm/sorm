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
class OptionTupleSupportSuite extends FunSuite with ShouldMatchers {

  import OptionTupleSupportSuite._


  test("saving goes ok"){

    db.save(A( None ))
    db.save(A( Some(2 -> None) ))
    db.save(A( Some(56 -> Some("asdf")) ))

  }
  test("saved entities are correct"){

    db.fetchById[A](1).get.a should be === None
    db.fetchById[A](2).get.a should be === Some(2 -> None)
    db.fetchById[A](3).get.a should be === Some(56 -> Some("asdf"))
  }

}
object OptionTupleSupportSuite {

  case class A
    ( a : Option[(Int, Option[String])] )

  val db
    = new Instance( Entity[A]() :: Nil,
                    "jdbc:h2:mem:test",
                    initMode = InitMode.DropAllCreate )

}