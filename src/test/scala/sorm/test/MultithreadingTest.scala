package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import sorm.samples.TestingInstance
import sorm.Entity

object MultithreadingTest {
  case class A (a : Int)
}
@RunWith(classOf[JUnitRunner])
class MultithreadingTest extends FunSuite with ShouldMatchers {
  import MultithreadingTest._

  TestingInstances.instances( Set() + Entity[A]() ) foreach { case (db, dbId) =>

    val db = TestingInstance.mysql(Entity[A](unique = Set() + Seq("a")))

    val a1 = db.save(A(1))
    val a2 = db.save(A(3))
    val a3 = db.save(A(0))
    val a4 = db.save(A(3000))

    test(dbId + " - Parallel queries"){
      Seq(0,1,2,3).par.flatMap{ i => db.access[A].whereEqual("a", i).fetchOne() }.seq
        .should(contain(a1) and contain(a3) and not contain(a4))
    }
    test(dbId + " - Parallel saving"){
      pending
    }

  }
}
