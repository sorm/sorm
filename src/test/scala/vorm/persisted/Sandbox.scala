package vorm.persisted


import someOtherPackage.domain.{Genre, Artist}

object Sandbox extends App {

  //  val persisted = new Artist("Metallica", Set()) with Persisted {
  //    val key = "23lkjds"
  //  }
  //
  //  val persisted2 = new Artist("Nirvana") with Persisted {
  //    val key = "123"
  //  }

  val artist = Artist("Nirvana", Set(Genre("rock"), Genre("grunge")))

  import PersistedEnabler._

  val persisted = toPersisted(artist, "some-key")

  assert(persisted.isInstanceOf[Persisted])
  assert(persisted.isInstanceOf[Artist])
  assert(persisted.key == "some-key")
  assert(persisted.name == "Nirvana")

  val copy = persisted.copy(name = "Foo Fighters")

  println(copy.isInstanceOf[Persisted])
  println(copy.isInstanceOf[Artist])

}


