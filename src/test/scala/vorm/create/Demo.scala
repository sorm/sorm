package vorm.create

import vorm._
import reflection._
import structure._
import mapping._
import extensions._

object Demo extends App {

  import samples.ArtistModelWithIds._

  val settings
    = Map(
        Reflection[Artist] →
        EntitySettings( Seq("id"), autoIncrement = Set("id") ),

        Reflection[Style] →
        EntitySettings( Seq("id"), autoIncrement = Set("id") ),

        Reflection[Name] →
        EntitySettings( Seq("id"), autoIncrement = Set("id"), indexes = Set(Seq("value")) ),

        Reflection[Locale] →
        EntitySettings( Seq("id"), autoIncrement = Set("id"), indexes = Set(Seq("code")) )
      )

  val mappings
    = settings.keys.map{ r => r -> new EntityMapping(None, r, settings) }.toMap

  ddl(mappings.values).println()

}
