package vorm.reflection


object Sandbox extends App {
  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def sings(song: String) = song + "sldfjsldkjf"
  }

  val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))

  assert(typeSymbol[Artist].t.name == "Artist")
  assert(typeSymbol[Set[String]].t.name == "Set")
  assert(typeSymbol[Artist].t.properties("genres").name == "Set")
  assert(typeSymbol[Artist].properties("genres").t.name == "Set")
  assert(typeSymbol[Artist].properties("genres").name == "genres")


  typeSymbol[Artist].properties.values.foreach(println)
  typeSymbol[Artist].properties("genres").t.generics.foreach(println)

  println(typeSymbol[Artist].properties("tags").value(artist))

  println(typeSymbol[Artist].methods("sings").result(artist, List("AAAA")))


}
