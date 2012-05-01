package vorm.persisted


import PersistedEnabler._

object Sandbox extends App {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)


  val artist = Artist("Nirvana", Set(Genre("rock"), Genre("grunge")))


  val persisted = toPersisted(artist, "some-key")

  assert(persisted.isInstanceOf[Persisted])
  assert(persisted.isInstanceOf[Artist])
  assert(persisted.key == "some-key")
  assert(persisted.name == "Nirvana")
  assert(persisted == artist)  //  an interesting and useful effect


  val copy = persisted.copy(name = "Puddle of Mudd")

  assert(copy.isInstanceOf[Persisted])
  assert(copy.isInstanceOf[Artist])
  //  the only problem: compiler thinks that `copy` does not implement `Persisted`, so to access `key` we have to specify it manually:
  assert(copy.asInstanceOf[Artist with Persisted].key == "some-key")
  assert(copy.name == "Puddle of Mudd")
  assert(copy != persisted)





}


