package vorm.mirrorQuirks



object Sandbox extends App {
  case class Genre(name: String)



}

class AA {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)

}
