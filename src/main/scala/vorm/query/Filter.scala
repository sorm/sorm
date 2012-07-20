package vorm.query

import vorm._
import reflection._

trait FilterNode {
}

object FilterNode {

  trait Filter extends FilterNode {
    def mapping : Mapping
  }


  case class Equals( mapping : Mapping, value: Any ) extends Filter
  case class NotEquals(property: String, value: Any) extends Filter

  case class Bigger
    ( mapping : Mapping,
      value : Any )
    extends Filter

  case class In
    ( mapping : Mapping,
      value : Any )
    extends Filter

  case class Contains
    ( mapping : Mapping,
      value : Any )
    extends Filter



  //  case class More(property: String, value: Any) extends Filter
  //  case class MoreIncluding(property: String, value: Any) extends Filter
  //  case class Less(property: String, value: Any) extends Filter
  //  case class LessIncluding(property: String, value: Any) extends Filter
  //  case class Like(property: String, value: Any) extends Filter
  //  case class Regex(property: String, value: Any) extends Filter
  //  case class In(property: String, value: Any) extends Filter

  case class Or(left: FilterNode, right: FilterNode) extends FilterNode
  case class And(left: FilterNode, right: FilterNode) extends FilterNode

}
