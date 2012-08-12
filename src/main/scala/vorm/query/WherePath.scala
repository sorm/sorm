package vorm.query

import vorm._
import query._
import structure._
import mapping._
import api._
import Query._

object WherePath {
  def apply
    ( host : Mapping,
      path : String,
      value : Any,
      operator : Operator )
    : Where
    = apply( host, parts(path), value, operator )

  private def apply
    ( host : Mapping,
      path : Seq[Part],
      value : Any,
      operator : Operator )
    : Where
    = ( host, path ) match {
        case ( host : MapMapping, Part.Key( key ) +: _ ) =>
          And(
            Filter(Operator.Equals, host.key, key),
            apply(host.value, path.tail, value, operator)
          )
        case ( host : SeqMapping, Part.Index( index ) +: _ ) =>
          And(
            Filter(Operator.Equals, host.index, index),
            apply(host.value, path.tail, value, operator)
          )
        case ( host, path ) =>
          Filter( operator, MappingPath(host, path), value )
      }


  private trait Part
  private object Part {
    case class Property ( name : String ) extends Part
    case class Key ( name : String ) extends Part
    case class Index ( index : Int ) extends Part
  }

  private def parts
    ( p : String )
    : Stream[Part]
    = ???



  // def apply
  //   ( host : Mapping,
  //     path : Seq[Part.Part],
  //     value : Any,
  //     operator : Operator )
  //   : Where
  //   = ( host, path ) match {
  //       case ( _, Seq() ) =>
  //         Filter(operator, host, value)
  //       case ( host : MapMapping, Part.Key( key ) +: Seq() ) =>
  //         And(
  //           Filter(Operator.Equals, host.key, key),
  //           apply(host.value, path.tail, value, operator)
  //         )
  //       case ( host : MapMapping, 
  //              Part.Property("size") +: Seq() |
  //              Part.Property("keys") +: Part.Property("size") +: Seq() |
  //              Part.Property("values") +: Part.Property("size") +: Seq() ) =>
  //         Filter(Operator.HasSize, host, value)
  //       case ( host : MapMapping, Part.Property("keys") +: Seq() ) =>
  //         ( operator, value ) match {
  //           case (Operator.Equals, value : Traversable[_]) => 
  //             value.view
  //               .map{ Filter(Operator.Equals, host.key, _) }
  //               .reduceOption{ Or }
  //               .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
  //           case (Operator.Includes, value : Traversable[_]) 
  //             if value.size > 0 =>
  //             value.view
  //               .map{ Filter(Operator.Equals, host.key, _) }
  //               .reduce{ Or }
  //           case (Operator.Contains, value : Any) =>
  //             Filter(Operator.Equals, host.key, value)
  //         }

  //       case ( host : SeqMapping, Seq() ) =>
  //         ( operator, value ) match {
  //           case (Operator.Equals, value : Seq[_]) =>
  //             value.view
  //               .zipWithIndex
  //               .map{ case (v, i) => 
  //                 And(
  //                   Filter(Operator.Equals, host.item, v), 
  //                   Filter(Operator.Equals, host.index, i) 
  //                 )
  //               }
  //               .reduceOption{ Or }
  //               .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
  //         }
  //     }
}
