package vorm.reflection

import Reflection._

object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String])

  tpe[Artist].properties.foreach(println)

  println(tpe(Genre("")))

  tpe[Artist].property("genres").get.tpe.generics.foreach(println)

}
