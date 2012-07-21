package vorm.query

import vorm._
import reflection._
import structure._

sealed trait WhereNode
object WhereNode {


  sealed trait Filter extends WhereNode {
    def mapping : Mapping
    def value : Any
  }

  sealed case class Equals
    ( mapping : Mapping,
      value: Any )
    extends Filter

  sealed case class NotEquals
    ( mapping : Mapping,
      value: Any )
    extends Filter

  sealed case class Bigger
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class BiggerIncluding
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class Smaller
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class SmallerIncluding
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class In
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class Contains
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class Like
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class Regex
    ( mapping : Mapping,
      value : Any )
    extends Filter


  sealed trait Composite extends WhereNode

  sealed case class And
    ( left: WhereNode, 
      right: WhereNode )
    extends Composite

  sealed case class Or
    ( left: WhereNode, 
      right: WhereNode )
    extends Composite
  
}
