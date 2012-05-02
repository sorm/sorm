package vorm.reflection

import Reflection._

object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String): Boolean = false
  }

  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))


  tpe[Artist].properties.foreach(println)
  tpe[Artist].property("genres").tpe.generics.foreach(println)


  println {
    tpe[Artist].method("sings").invoke(artist, List("ass"))
//    tpe[Artist].property("tags").value(artist)
  }

}
