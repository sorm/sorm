package vorm.create

import vorm._
import reflection._
import structure._
import mapping._
import extensions._

object Demo extends App {

  import samples.ArtistModelWithIds._
  import structure.Sample._

  ddl(mappings.values).println()

}
