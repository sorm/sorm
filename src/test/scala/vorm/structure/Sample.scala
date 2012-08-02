package vorm.structure

import vorm._
import reflection._
import mapping._

object Sample {

  import samples.ArtistModelWithIds._

  val settings
    = Map(
          Reflection[Artist] → 
          EntitySettings( Seq("id"), autoIncrement = Set("id") ),

          Reflection[Style] → 
          EntitySettings( Seq("id"), autoIncrement = Set("id") ),

          Reflection[Name] → 
          EntitySettings( Seq("id"), autoIncrement = Set("id") ),

          Reflection[Locale] → 
          EntitySettings( Seq("id"), autoIncrement = Set("id") )
        )

  val mappings
    = settings.keys.map{ r => r -> new EntityMapping(None, r, settings) }.toMap

}
