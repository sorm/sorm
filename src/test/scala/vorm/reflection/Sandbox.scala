package vorm.reflection


object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String) = song + "sldfjsldkjf"
  }

  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))

  assert(reflection[Artist].name == "Artist", reflection[Artist].name)
  assert(reflection[Set[String]].name == "Set")
  assert(reflection[Artist].property("genres").tpe.name == "Set")

  assert(reflection(Seq(342, 34)).is[AnyVal] == false)
  assert(reflection(342).is[AnyVal] == true)


  reflection[Artist].properties.foreach(println)
  reflection[Artist].property("genres").tpe.generics.foreach(println)

  println(reflection[Artist].property("tags").value(artist))

  println {
    reflection[Artist].method("sings").invoke(artist, List("AAAA"))
  }

}
