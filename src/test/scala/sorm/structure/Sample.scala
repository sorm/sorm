package sorm.structure

import sorm._
import reflection._
import mapping._

object Sample {

  import samples.ArtistModelWithIds._

  val settings
    = Map(
          Reflection[Artist] → EntitySettings(),
          Reflection[Style] → EntitySettings(),
          Reflection[Name] → EntitySettings(),
          Reflection[Locale] → EntitySettings()
        )

  val mappings
    = settings.keys.map{ r => r -> new EntityMapping(None, r, settings) }.toMap

}
