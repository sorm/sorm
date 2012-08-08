package vorm.api

import org.joda.time._

import vorm._
import extensions._

object Demo extends App {

  import samples.ArtistModel._

  val db
    = new Instance(
        url
          = "jdbc:h2:mem:test",
        user
          = "",
        password
          = "",
        entities
          = Seq( Entity[Artist](), Entity[Style](), Entity[Name](), Entity[Locale]() )
      )

  val ru = db.save(Locale("ru"))
  val en = db.save(Locale("en"))
  val rock = db.save(Style(Name(en, "rock") :: Nil))
  val metallica = db.save( Artist( Seq(Name(en, "Metallica")), Set(rock) ) )

  metallica.id.println()
//  val artist = api.all[Artist].filterEquals("name", "Metallica").offset(0).limit(1).head
//
//  val style
//    = db.one[Style].where(
//        ("names.value" === "pop") and
//        ("names.locale.code" === "en")
//      )
//


}
