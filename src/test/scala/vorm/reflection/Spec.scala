package vorm.reflection

import org.specs2.mutable._

class Spec extends Specification {

  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String) = song + "sldfjsldkjf"
  }

  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))

  val r = reflection[Artist]
  "type name" should {
    "be correct" in {
      r.t.name mustEqual "Artist"
    }
    "work on types with generics" in {
      reflection[Set[String]].t.name === "Set"
    }
    "should be gettable on properties" in {
      r.t.properties("genres").name === "Set"
      r.properties("genres").t.name === "Set"
    }
  }

  "property" should {
    "have correct name" in {
      reflection[Artist].properties("genres").name === "genres"
    }
  }



}