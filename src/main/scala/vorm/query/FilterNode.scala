package vorm.query

import vorm._
import reflection._

sealed trait FilterNode {
}

object FilterNode {

  sealed trait Filter extends FilterNode {
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
      value : Any,
      including : Boolean )
    extends Filter

  sealed case class SmallerIncluding
    ( mapping : Mapping,
      value : Any,
      including : Boolean )
    extends Filter

  sealed case class In
    ( mapping : Mapping,
      value : Any )
    extends Filter

  sealed case class Contains
    ( mapping : Mapping,
      value : Any )
    extends Filter





  //  sealed case class More(property: String, value: Any) extends Filter
  //  sealed case class MoreIncluding(property: String, value: Any) extends Filter
  //  sealed case class Less(property: String, value: Any) extends Filter
  //  sealed case class LessIncluding(property: String, value: Any) extends Filter
  //  sealed case class Like(property: String, value: Any) extends Filter
  //  sealed case class Regex(property: String, value: Any) extends Filter
  //  sealed case class In(property: String, value: Any) extends Filter

  sealed case class Or(left: FilterNode, right: FilterNode) extends FilterNode
  sealed case class And(left: FilterNode, right: FilterNode) extends FilterNode

}
