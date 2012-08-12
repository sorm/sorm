package vorm.select

import vorm._
import reflection._
import structure._
import mapping._
import extensions._
import query.Query._

object Demo extends App {

  import samples.ArtistModelWithIds._
  import structure.Sample._


  val mapping
    = mappings(Reflection[Artist])

//  val pkSelect
//    = MappingSelect(mapping)
//      .primaryKey
//      .withFilter(
//        Where.In(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping]
//            .item.asInstanceOf[EntityMapping]
//            .properties("value"),
//          "Nirvana"
//        )
//      )
//      .withFilter(
//        Where.Equals(
//          mapping
//            .properties("names").asInstanceOf[SeqMapping]
//            .item.asInstanceOf[EntityMapping]
//            .properties("value"),
//          "Nirvana"
//        )
//      )
//      .withFilter(
//        Where.Equals(
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
//          Order(mapping.properties("names"))::
//          Order(
//            mapping
//              .properties("names").asInstanceOf[SeqMapping]
//              .item.asInstanceOf[EntityMapping]
//              .properties("value")
//          ) ::
//          Nil
//        )


  val query = Query(
    Kind.Select,
    mapping,
    Some(
      Or(
        Filter(
          mapping
            .properties("names").asInstanceOf[SeqMapping],
          Seq(),
          Operator.Equals
        ),
        Filter(
          mapping
            .properties("names").asInstanceOf[SeqMapping]
            .item.asInstanceOf[EntityMapping]
            .properties("value"),
          "Nirvana",
          Operator.Equals
        )
      )
    ),
    Order(
      mapping
        .properties("names").asInstanceOf[SeqMapping]
        .item.asInstanceOf[EntityMapping]
        .properties("value"),
      true
    ) :: Nil,
    Some(4),
    5
  )

  query.statementAndResultMappings.trace()
}
