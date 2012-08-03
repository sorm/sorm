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


  val mapping
    = mappings(Reflection[Artist])

//  new MappingSelect(mapping)
//    .withFilter(
//      Query.Where.In(
//        mapping
//          .properties("names").asInstanceOf[SeqMapping]
//          .item.asInstanceOf[EntityMapping]
//          .properties("value"),
//        "Nirvana"
//      )
//    )
//    .withFilter(
//      Query.Where.Equals(
//        mapping
//          .properties("names").asInstanceOf[SeqMapping]
//          .item.asInstanceOf[EntityMapping]
//          .properties("value"),
//        "Nirvana"
//      )
//    )
//    .withFilter(
//      Query.Where.Equals(
//        mapping
//          .properties("names").asInstanceOf[SeqMapping],
//        Seq()
//      )
//    )
//    .sql
//    .rendering
//    .println()

  new MappingSelect(mapping)
    .resultSet
//    .withSelect()
    .sql.rendering.println()

//  val query = Query(
//    Query.Kind.Select,
//    mapping,
//    Some(
//      Query.Where.Or(
//        Query.Where.Equals(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping],
//          Seq()
//        ),
//        Query.Where.Equals(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping]
//            .item.asInstanceOf[EntityMapping]
//            .properties("value"),
//          "Nirvana"
//        )
//      )
//    )
//  )
//  MappingSelect(query).sql.rendering.println()
}
