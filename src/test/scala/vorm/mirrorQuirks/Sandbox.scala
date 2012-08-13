package vorm.mirrorQuirks

import MirrorQuirks._

object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(name: String, genres: Set[Genre])
  trait Persisted

  val t = tag[Artist with Persisted].tpe


  println(mixinBasis(t))
}

