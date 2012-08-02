package vorm.select

import vorm._
import reflection._
import structure._
import mapping._
import extensions._
import query._

object Demo extends App {

  import samples.ArtistModelWithIds._
  import structure.Sample._


  val artistMapping
    = mappings(Reflection[Artist])

  new MappingSelect(artistMapping)
    .withFilter(
      Query.Where.In(
        artistMapping
          .properties("names").asInstanceOf[SeqMapping]
          .item.asInstanceOf[EntityMapping]
          .properties("value"),
        "Nirvana"
      )
    )
    .withFilter(
      Query.Where.Equals(
        artistMapping
          .properties("names").asInstanceOf[SeqMapping]
          .item.asInstanceOf[EntityMapping]
          .properties("value"),
        "Nirvana"
      )
    )
    .sql
    .rendering
    .println()
}
