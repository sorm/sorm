package vorm.select

import vorm._
import reflection._
import structure._
import extensions._

object Demo extends App {

  import samples.ArtistModelWithIds._
  import structure.Sample._

  val artistMapping
    = root.children.find{ _.reflection == Reflection[Artist] }.get.child
        .asInstanceOf[mapping.Entity]

  new MappingSelect(artistMapping)
    .sql
    .sql
    .println()
}
