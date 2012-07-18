package vorm.reflection

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite with ShouldMatchers {
  object Container {
    case class Genre(name: String)
    val t = tpe[Genre]
  }
  test("tpe returns an equaling Type when called from different contexts") {
    val t1 = tpe[Container.Genre]
    val t2 = Container.t

    t1 should equal (t2)
    t1.hashCode should equal (t2.hashCode)
//    t1 should be theSameInstanceAs (t2)

  }
  test("tpe returns different Type`s for types with different generics") {
    tpe[Seq[Int]] should not equal(tpe[Seq[String]])
    tpe[Seq[Int]] should not equal(tpe[Seq[Any]])
  }
  test("tpe returns the same Type with for types with same generics") {
    tpe[Seq[Int]] should equal(tpe[Seq[Int]])
  }
  test("type reflection extensions") {
    Seq(1,1,3).tpe should equal(tpe[Seq[Int]])
    Seq(1,1,3).tpe should not equal(tpe[Seq[Long]])
  }

}
