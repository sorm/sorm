package vorm.sql


sealed trait WhereNode
object WhereNode {


  sealed trait Filter[ValueT] extends WhereNode {
    def identifier : String
    def value : ValueT
  }

  sealed case class Equals
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class NotEquals
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class Bigger
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class BiggerIncluding
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class Smaller
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class SmallerIncluding
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class In
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class Contains
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class Like
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]

  sealed case class Regex
    [ ValueT ]
    ( identifier : String,
      value : ValueT )
    extends Filter[ValueT]


  sealed trait Composite extends WhereNode

  sealed case class And
    ( left : WhereNode,
      right : WhereNode )
    extends Composite

  sealed case class Or
    ( left : WhereNode,
      right : WhereNode )
    extends Composite
  
}
