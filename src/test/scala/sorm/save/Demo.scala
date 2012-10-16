package sorm.save

import sorm._
import jdbc._
import reflection._
import structure._
import mapping._
import persisted._
import sext._

import reflect.basis._

object Demo extends App {

  import samples.ArtistModel._


  val db
    = new ConnectionAdapterSimulator with SaveAdapter {
        val settings
          = Map(
              Reflection[Artist] → EntitySettings(),
              Reflection[Style] → EntitySettings(),
              Reflection[Name] → EntitySettings(),
              Reflection[Locale] → EntitySettings()
            )

        val mappings
          = settings.keys.map{ r => r -> new EntityMapping(None, r, settings) }.toMap

        def save[T <: AnyRef : TypeTag](v : T)
          = saveEntityAndGetIt(v, mappings(Reflection[T]))
              .asInstanceOf[T with Persisted]
      }

  val ru = db.save(Locale("ru"))
  val en = db.save(Locale("en"))
  val rock = db.save(Style(Name(en, "rock") :: Nil))
  val metallica = db.save( Artist( Seq(Name(en, "Metallica")), Set(rock) ) )

  metallica.id.trace()
}
