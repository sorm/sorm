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


    val persisted = toPersisted(artist, "some-key")

    assert(persisted.isInstanceOf[Persisted])
    assert(persisted.isInstanceOf[Artist])
    assert(persisted.key == "some-key")
    assert(persisted.name == "Nirvana")
    assert(persisted == artist) //  an interesting and useful effect


    val copy = persisted.copy(name = "Puddle of Mudd")

    assert(copy.isInstanceOf[Persisted])
    assert(copy.isInstanceOf[Artist])
    //  the only problem: compiler thinks that `copy` does not implement `Persisted`, so to access `key` we have to specify it manually:
    assert(copy.asInstanceOf[Artist with Persisted].key == "some-key")
    assert(copy.name == "Puddle of Mudd")
    assert(copy != persisted)


  }
}
object Suite {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)
}