package vorm.api

import org.joda.time._

import vorm._
import extensions._
import com.codahale.logula.Logging
import org.apache.log4j.Level

object Demo extends App {

  import samples.ArtistModel._

  Logging.configure { log =>
    log.level = Level.TRACE
  }

  val db
    = new Instance(
        url
//          = "jdbc:h2:mem:test",
          = "jdbc:mysql://localhost/test",
        user
          = "",
        password
          = "",
        entities
          = Seq( Entity[Artist](), Entity[Style](), Entity[Name](), Entity[Locale]() ),
        mode
          = Mode.DropCreate
      )

  val ru
    = db.save( Locale("ru") )
  val en
    = db.save( Locale("en") )
  val rock
    = db.save( Style(Name(en, "Rock") :: Nil) )
  val hardRock
    = db.save( Style(Name(en, "Hard Rock") :: Name(ru, "Тяжёлый рок") :: Nil) )
  val metallica
    = db.save( Artist( Seq(Name(en, "Metallica")), Set(rock, hardRock) ) )
  val nirvana
    = db.save( Artist( Seq(Name(en, "Nirvana")), Set(rock, hardRock) ) )
  val kino
    = db.save( Artist( Seq(Name(ru, "Кино"), Name(en, "Kino")), Set(rock) ) )

  kino.id.println()
  kino.println()
//  val artist = api.all[Artist].filterEquals("name", "Metallica").offset(0).limit(1).head
//
//  val style
//    = db.one[Style].where(
//        ("names.value" === "pop") and
//        ("names.locale.code" === "en")
//      )
//


}
