package vorm.persisted

import vorm._
import reflection._
import extensions._

object Sandbox extends App {

  case class Genre(name: String)
  case class Artist(name: String, amazonId: Option[String], genres: Set[Genre], tags: Set[String])

  ClassCreator.code(Reflection[Genre], "A").println()
  ClassCreator.code(Reflection[Artist], "B").println()

  val artist = Artist("Nirvana", Some("saldkfj"), Set(Genre("grunge"), Genre("rock")), Set("kurt", "cobain"))

  val p = persisted(artist, 4)
  val p1 = persisted(p, 2)

  println(p1)
}