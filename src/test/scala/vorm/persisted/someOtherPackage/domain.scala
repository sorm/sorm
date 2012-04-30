package vorm.persisted.someOtherPackage

object domain {
  case class Artist(name: String, genres: Set[Genre])
  case class Genre(name: String)
}