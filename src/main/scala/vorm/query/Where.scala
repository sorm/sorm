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
          Filter(operator, host, value)
        case ( host : MapMapping, Path.Key( key ) +: Seq() ) =>
          And(
            Filter(Operator.Equals, host.key, key),
            apply(host.value, path.tail, value, operator)
          )
        case ( host : MapMapping, Path.Property("size") +: Seq() ) =>
          Filter(Operator.HasSize, host, value)
        case ( host : MapMapping, Path.Property("keys") +: Seq() ) =>
          ( operator, value ) match {
            case (Operator.Equals, value : Traversable[_]) =>
              value.view
                .map{ Filter(Operator.Equals, host.key, _) }
                .reduceOption{ Or }
                .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
          }
      }
}
