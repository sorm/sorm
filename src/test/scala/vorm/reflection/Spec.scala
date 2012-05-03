package vorm.reflection

import org.specs2.mutable._

class Spec extends Specification{
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String) = song + "sldfjsldkjf"
  }
  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))


  "A property type" should {
    "have proper generics" in
      tpe[Artist].property("genres").t.generics.head === tpe[Genre]
    "have proper generics on primitive types" in
      tpe[Artist].property("tags").t.generics.head === tpe[String]
  }
  "A type" should {
    val t = tpe[Map[String, List[Genre]]]
    "have proper primitive generics" in
      tpe[Set[String]].generics.head === tpe[String]
  }

}
