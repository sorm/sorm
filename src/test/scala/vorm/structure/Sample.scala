package vorm.structure

import vorm._
import reflection._

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
  
  val root
    = new mapping.Root(settings)

}
