package vorm.reflection

import org.specs2.mutable._

class Spec extends Specification{
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String) = song + "sldfjsldkjf"
  }
  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))

  "A Seq[Int] type" should {
    val t = tpe[Seq[Int]]
    
    "inherit Seq[Int]" in
      t.inherits[Seq[Int]]
    "inherit Traversable[_]" in
      t.inherits[Traversable[_]]
    "inherit Traversable[Int]" in
      t.inherits[Traversable[Int]]
    "not iherit AnyVal" in 
      !t.inherits[AnyVal]
    "inherit Any" in 
      t.inherits[Any]
    "have Int as the only generic" in {
      t.generics.length === 1 
      t.generics(0) == tpe[Int]
    }
  }

  "A type" should {
    "have proper primitive generics" in
      tpe[Set[String]].generics.head === tpe[String]

  }

  "A property" should {
    "have proper type generics" in
      tpe[Artist].property("genres").t.generics.head === tpe[Genre]
    "have proper type generics on primitive types" in
      tpe[Artist].property("tags").t.generics.head === tpe[String]
  }
}
