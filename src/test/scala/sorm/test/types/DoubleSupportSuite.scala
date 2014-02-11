package sorm.test.types

import org.scalatest._
import sorm._, test._

@org.junit.runner.RunWith(classOf[junit.JUnitRunner])
class DoubleSupportSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {
  import DoubleSupportSuite._

  def entities = Set(Entity[A]())
  instancesAndIds foreach { case (db, dbId) =>
    val seq : Seq[Double] = Seq(2, 2.230192321, 3.3209483290840923839230, 0.213)
    seq.foreach(v => db.save(A(v)))
    test(dbId + " - fetching"){
      db.query[A].order("id").fetch().map(_.a)
        .should(equal(seq))
    }
    test(dbId + " - filtering"){
      db.query[A].whereLarger("a", 2).fetch()
        .should(have('size(seq.count(_ > 2))))
    }
  }
}
object DoubleSupportSuite {
  case class A ( a : Double )
}
