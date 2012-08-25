package sorm.persisted

import sorm._
import reflection._
import extensions.Extensions._

object Sandbox extends App {

  case class Genre(name: String)
  case class Artist(name: String, amazonId: Option[String], genres: Set[Genre], tags: Set[String])

  PersistedClass.code(Reflection[Genre], "A").trace()
  PersistedClass.code(Reflection[Artist], "B").trace()

  val artist = Artist("Nirvana", Some("saldkfj"), Set(Genre("grunge"), Genre("rock")), Set("kurt", "cobain"))

  val p = Persisted(artist, 4)
  val p1 = Persisted(p, 2)

  println(p1)
}