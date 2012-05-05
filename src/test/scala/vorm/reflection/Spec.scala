package vorm.reflection

import org.specs2.mutable._

class Spec extends Specification{
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String) = song + "sldfjsldkjf"
  }
  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))
  "An Int type" should {
    "inherit AnyVal" in
      tpe[Int].inherits[AnyVal]
    "inherit Any" in
      tpe[Int].inherits[Any]
    "not inherit AnyRef" in
      !tpe[Int].inherits[AnyRef]
    "inherit Int" in
      tpe[Int].inherits[Int]
  }
  "A String type" should {
    "inherit Any" in
      tpe[String].inherits[Any]
    "inherit String" in
      tpe[String].inherits[String]
  }
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
    val s = tpe[String]
    val t = tpe[Artist]
    "have proper type generics" in
      t.property("genres").t.generics.head === tpe[Genre]
    "have proper type generics on primitive types" in
      t.property("tags").t.generics.head === s
  }
  "A map of ints by strings Type" should {
    val t = tpe[Map[String, Int]]
    "inherit an unparameterized map" in
      t.inherits[Map[_, _]]
    "inherit an int by string map" in
      t.inherits[Map[String, Int]]
    "not inherit an int by int map" in
      !t.inherits[Map[Int, Int]]
  }
  "A List of Ints by Set of Strings Map Type" should {
    val t = tpe[Map[Set[String], List[Int]]]
    "inherit the same type" in
      t.inherits[Map[Set[String], List[Int]]]
    "inherit the same main type with any generics" in
      t.inherits[Map[_, _]]
    "not inherit the same main type with different generics" in
      !t.inherits[Map[Int, String]]
    "not inherit the same main type with different deep generics" in
      !t.inherits[Map[Set[Int], List[String]]]
  }
  "A Set of Ints Type" should {
    val t = tpe[Set[Int]]
    "inherit the same type" in
      t.inherits[Set[Int]]
    "inherit the same main type with any generics" in
      t.inherits[Set[_]]
    "not inherit the same main type with different generics" in
      !t.inherits[Set[String]]
  }
}
