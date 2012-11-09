package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sext._, embrace._

@RunWith(classOf[JUnitRunner])
class OptionValueSupportSuite extends FunSuite with ShouldMatchers {

  import OptionValueSupportSuite._

  TestingInstances.instances(Set() + Entity[A]()) foreach { case (db, dbId) =>

    test(dbId + " - saving goes ok"){
      db.save(A(None))
      db.save(A(Some(3)))
      db.save(A(Some(7)))
    }
    test(dbId + " - saved entities are correct"){
      db.fetchById[A](1).a should be === None
      db.fetchById[A](2).a should be === Some(3)
      db.fetchById[A](3).a should be === Some(7)
    }
    test(dbId + " - equals filter"){
      db.access[A]
        .whereEqual("a", None).fetchOne().get.id should be === 1
      db.access[A]
        .whereEqual("a", Some(3)).fetchOne().get.id should be === 2
    }
    test(dbId + " - not equals filter"){
      db.access[A]
        .whereNotEqual("a", None)
        .fetch().map{_.id.toInt}.toSet
        .should( not contain (1) and contain (3) and contain (2) )
      db.access[A]
        .whereNotEqual("a", Some(3))
        .fetch().map{_.id.toInt}.toSet
        .should( not contain (2) and contain (1) and contain (3) )
    }
    
  }

}
object OptionValueSupportSuite {

  case class A
    ( a : Option[Int] )

}