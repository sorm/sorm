package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sext._, embrace._

@RunWith(classOf[JUnitRunner])
class OptionTupleSupportSuite extends FunSuite with ShouldMatchers {

  import OptionTupleSupportSuite._

  TestingInstances.instances(Set() + Entity[A]()) foreach { case (db, dbId) =>
    db.save(A( None ))
    db.save(A( Some(2 -> None) ))
    db.save(A( Some(56 -> Some("asdf")) ))

    test(dbId + " - top none"){
      db.fetchById[A](1).a should be === None
    }
    test(dbId + " - deep none"){
      db.fetchById[A](2).a should be === Some(2 -> None)
    }
    test(dbId + " - deep some"){
      db.fetchById[A](3).a should be === Some(56 -> Some("asdf"))
    }
  }

}
object OptionTupleSupportSuite {

  case class A
    ( a : Option[(Int, Option[String])] )

}