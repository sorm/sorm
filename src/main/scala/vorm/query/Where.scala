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
        case ( _, Seq() ) 
          =>
          Filter(operator, host, value)
        case ( host : MapMapping, Path.Key( key ) +: Seq() ) 
          =>
          And(
            Filter(Operator.Equals, host.key, key),
            apply(host.value, path.tail, value, operator)
          )
        case ( host : MapMapping, 
               Path.Property("size") +: Seq() | 
               Path.Property("keys") +: Path.Property("size") +: Seq() |
               Path.Property("values") +: Path.Property("size") +: Seq() ) => 
          Filter(Operator.HasSize, host, value)
        case ( host : MapMapping, Path.Property("keys") +: Seq() ) =>
          ( operator, value ) match {
            case (Operator.Equals, value : Traversable[_]) => 
              value.view
                .map{ Filter(Operator.Equals, host.key, _) }
                .reduceOption{ Or }
                .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
            case (Operator.Includes, value : Traversable[_]) 
              if value.size > 0 =>
              value.view
                .map{ Filter(Operator.Equals, host.key, _) }
                .reduce{ Or }
            case (Operator.Contains, value : Any) =>
              Filter(Operator.Equals, host.key, value)
          }

        case ( host : SeqMapping, Seq() ) =>
          ( operator, value ) match {
            case (Operator.Equals, value : Seq[_]) =>
              value.view
                .zipWithIndex
                .map{ case (v, i) => 
                  And(
                    Filter(Operator.Equals, host.item, v), 
                    Filter(Operator.Equals, host.index, i) 
                  )
                }
                .reduceOption{ Or }
                .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
          }
      }
}
