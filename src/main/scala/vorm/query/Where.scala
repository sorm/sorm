package vorm.query

import vorm._
import query._
import structure._
import mapping._
import Query._

object Where {
  def apply
    ( host : Mapping,
      path : String,
      value : Any,
      operator : Operator )
    : Where
    = apply( host, Path(path), value, operator )

  def apply
    ( host : Mapping,
      path : Seq[Path.Part],
      value : Any,
      operator : Operator )
    : Where
    = ( host, path ) match {
        case ( _, Seq() ) => 
          Filter(host, value, operator)
        case ( host : MapMapping, Path.Key( key ) +: Seq() ) =>
          And(
            Filter(host.key, key, Operator.Equals),
            apply(host.value, path.tail, value, operator)
          )
        case ( host : MapMapping, Path.Property("size") +: Seq() ) =>
          Filter(host, value, Operator.HasSize)
        case ( host : MapMapping, Path.Property("keys") +: Seq() ) =>
          ( operator, value ) match {
            case (Operator.Equals, value : Traversable[_]) =>
              value.view
                .map{ Filter(host.key, _, Operator.Equals) }
                .reduceOption{ Or }
                .foldLeft( Filter(host, value.size, Operator.HasSize) ){ And }
          }
      }
}
