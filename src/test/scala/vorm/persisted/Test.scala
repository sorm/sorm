package vorm.persisted

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import vorm.persisted.Test._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite with ShouldMatchers {


  test("all is fine") {


    val artist = Artist("Nirvana", Set(Genre("rock"), Genre("grunge")))


    val instance = Persisted(artist, 24)

    assert(instance.isInstanceOf[Persisted])
    assert(instance.isInstanceOf[Artist])
    assert(instance.id == 24)
    assert(instance.name == "Nirvana")
    assert(instance == artist) //  an interesting and useful effect


    val copy = instance.copy(name = "Puddle of Mudd")

    assert(copy.isInstanceOf[Persisted])
    assert(copy.isInstanceOf[Artist])
    //  the only problem: compiler thinks that `copy` does not implement `Persisted`, so to access `key` we have to specify it manually:
    assert(copy.asInstanceOf[Artist with Persisted].id == 24)
    assert(copy.name == "Puddle of Mudd")
    assert(copy != instance)


  }
  test("dynamic persisted") {
    val properties = Map("name" -> "Nirvana", "genres" -> Set(Genre("grunge")))
    val instance = Persisted[Artist](properties, 35)
    instance should have('name("Nirvana"))
    instance should have('genres(Set(Genre("grunge"))))
    instance should have('id(35))
  }
  test("dynamic persisted untyped iterable") {
    Persisted[Artist](Map("name" -> "", "genres" -> Set()), 0) should
    have(
      'name(""),
      'genres(Set()),
      'id(0)
    )
  }
  test("dynamic persisted fails on incorrect map") {
    evaluating {Persisted[Artist](Map("name" -> "Nirvana"), 35)} should produce[Exception]
  }
  test("persisted on persisted") {
    Persisted(Persisted(Artist("Nirvana", Set()), 2), 4).id should equal(4)
  }
}
object Test {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)
}