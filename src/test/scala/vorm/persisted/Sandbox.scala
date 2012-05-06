package vorm.persisted

import vorm.reflection._

object Sandbox extends App {
  println(tpe[AA#Artist].fullName)
}

class AA {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)

}
