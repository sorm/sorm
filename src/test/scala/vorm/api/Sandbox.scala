package vorm.api

object Sandbox extends App {

  val api: API

  case class Genre(
    name: String
  )

  case class Artist(
    name: String,
    genres: Set[Genre]
  )

  val artist = api.all[Artist].filterEquals("name", "Metallica").offset(0).limit(1).head

}
