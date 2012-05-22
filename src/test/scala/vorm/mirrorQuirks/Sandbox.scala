package vorm.mirrorQuirks


object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(name: String, genres: Set[Genre])
  trait Persisted
  class InheritingType extends Genre("")

  val t1 = tag[InheritingType].tpe
  val t2 = tag[Artist with Persisted].tpe

  val s = t2.typeSymbol

  println(t1.kind)
  println(t2.kind)

  println(name(tag[InheritingType].sym))
}

