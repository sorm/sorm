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
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.WARN
  }

  object Db
    extends Instance(
      url
        = "jdbc:h2:mem:test",
//        = "jdbc:mysql://localhost/test",
      user
        = "",
      password
        = "",
      entities
        = Seq( Entity[Artist](), Entity[Style](), Entity[Name](), Entity[Locale]() ),
      mode
        = Mode.DropAllCreate
    )

  val ru
    = Db.save( Locale("ru") )
  val en
    = Db.save( Locale("en") )
  val rock
    = Db.save( Style(Name(en, "Rock") :: Nil) )
  val hardRock
    = Db.save( Style(Name(en, "Hard Rock") :: Name(ru, "Тяжёлый рок") :: Nil) )
  val metallica
    = Db.save( Artist( Seq(Name(en, "Metallica")), Set(rock, hardRock) ) )
  val nirvana
    = Db.save( Artist( Seq(Name(en, "Nirvana")), Set(rock, hardRock) ) )
  val kino
    = Db.save( Artist( Seq(Name(ru, "Кино"), Name(en, "Kino")), Set(rock) ) )

//  kino.id.println()
//  kino.println()
//  Db.query[Name].filterEquals("value", "Rock").all.println()
  Db.query[Artist].all.map{_.names.head.value}.println()
  Db.query[Artist].all.map{_.id}.println()
//  val artist = api.all[Artist].filterEquals("name", "Metallica").offset(0).limit(1).head
//
//  val style
//    = db.one[Style].where(
//        ("names.value" === "pop") and
//        ("names.locale.code" === "en")
//      )
//


}
