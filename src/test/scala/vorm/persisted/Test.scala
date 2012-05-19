package vorm.persisted

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import vorm.persisted.Suite._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class Suite extends FunSuite with ShouldMatchers {


  test("all is fine") {


    val artist = Artist("Nirvana", Set(Genre("rock"), Genre("grunge")))


    val persisted = persisted(artist, 24)

    assert(persisted.isInstanceOf[Persisted])
    assert(persisted.isInstanceOf[Artist])
    assert(persisted.id == 24)
    assert(persisted.name == "Nirvana")
    assert(persisted == artist) //  an interesting and useful effect


    val copy = persisted.copy(name = "Puddle of Mudd")

    assert(copy.isInstanceOf[Persisted])
    assert(copy.isInstanceOf[Artist])
    //  the only problem: compiler thinks that `copy` does not implement `Persisted`, so to access `key` we have to specify it manually:
    assert(copy.asInstanceOf[Artist with Persisted].id == 24)
    assert(copy.name == "Puddle of Mudd")
    assert(copy != persisted)


  }

  test("dynamic persisted") {
    val properties = Map("name" -> "Nirvana", "genres" -> Set(Genre("grunge")))
    val instance = persisted[Artist](properties, 35)
    instance should have('name("Nirvana"))
    instance should have('genres(Set(Genre("grunge"))))
    instance should have('id(35))
  }
  test("dynamic persisted untyped iterable") {
    persisted[Artist](Map("name" -> "", "genres" -> Set()), 0) should
      have(
        'name(""),
        'genres(Set()),
        'id(0)
      )
  }
  test("dynamic persisted fails on incorrect map") {
    evaluating {persisted[Artist](Map("name" -> "Nirvana"), 35)} should produce[Exception]
    evaluating {persisted[Artist](Map("name" -> "", "genres" -> Set(), "sddfsdf" -> 0), 0)} should produce[Exception]
  }

}
object Suite {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)
}