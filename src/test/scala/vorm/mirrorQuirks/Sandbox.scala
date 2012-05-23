package vorm.mirrorQuirks


object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(name: String, genres: Set[Genre])
  trait Persisted

  val t = tag[Artist with Persisted].tpe


  println(mixinBasis(t))
}

