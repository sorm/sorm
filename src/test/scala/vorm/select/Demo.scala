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

//  val pkSelect
//    = MappingSelect(mapping)
//      .primaryKey
//      .withFilter(
//        Query.Where.In(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping]
//            .item.asInstanceOf[EntityMapping]
//            .properties("value"),
//          "Nirvana"
//        )
//      )
//      .withFilter(
//        Query.Where.Equals(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping]
//            .item.asInstanceOf[EntityMapping]
//            .properties("value"),
//          "Nirvana"
//        )
//      )
//      .withFilter(
//        Query.Where.Equals(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping],
//          Seq()
//        )
//      )
//
//  val rsSelect
//    = MappingSelect(mapping)
//        .resultSet
//        .withOrders(
//          Query.Order(mapping.properties("names"))::
//          Query.Order(
//            mapping
//              .properties("names").asInstanceOf[SeqMapping]
//              .item.asInstanceOf[EntityMapping]
//              .properties("value")
//          ) ::
//          Nil
//        )


  val query = Query(
    Query.Kind.Select,
    mapping,
    Some(
      Query.Where.Or(
        Query.Where.Equals(
          mapping
            .properties("names").asInstanceOf[SeqMapping],
          Seq()
        ),
        Query.Where.Equals(
          mapping
            .properties("names").asInstanceOf[SeqMapping]
            .item.asInstanceOf[EntityMapping]
            .properties("value"),
          "Nirvana"
        )
      )
    ),
    Query.Order(
      mapping
        .properties("names").asInstanceOf[SeqMapping]
        .item.asInstanceOf[EntityMapping]
        .properties("value"),
      true
    ) :: Nil,
    Some(4),
    5
  )

  query.statementAndResultMappings.println()
}
