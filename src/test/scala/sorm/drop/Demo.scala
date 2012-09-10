package sorm.drop

import sorm._
import reflection._
import structure._
import mapping._
import sext.Sext._
import util.Random

object Demo extends App {

  import samples.ArtistModel._
  import Drop._

  val settings
    = Map(
      Reflection[Artist] →
      EntitySettings(),

      Reflection[Style] →
      EntitySettings(),

      Reflection[Name] →
      EntitySettings(indexes = Set(Seq("value"))),

      Reflection[Locale] →
      EntitySettings(indexes = Set(Seq("code")))
    )

  val mappings
    = settings.keys.map {r => r -> new EntityMapping(None, r, settings)}.toMap

  ddl(mappings.values).trace()


}
