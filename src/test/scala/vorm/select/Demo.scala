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


//  val mapping
//    = mappings(Reflection[Name])
//
//  new MappingSelect(mapping)
//    .withSkeletonTo(
//      mapping.properties("locale")
//    )
//    .sql
//    .rendering
//    .println()


  val artistMapping
    = mappings(Reflection[Artist])

  new MappingSelect(artistMapping)
    .withSkeletonTo(
      artistMapping
          .properties("names").asInstanceOf[SeqMapping]
          .item.asInstanceOf[EntityMapping]
          .properties("locale")
    )
//    .withFilter(
//      Query.Where.In(
//        artistMapping
//          .properties("names").asInstanceOf[SeqMapping]
//          .item.asInstanceOf[EntityMapping]
//          .properties("value"),
//        "Nirvana"
//      )
//    )
    .sql
    .rendering
    .println()
}
