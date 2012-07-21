package vorm.query

import vorm._
import reflection._
import structure._

sealed trait WhereNode
object WhereNode {

  sealed case class Filter
    ( kind : Filter.Kind,
      mapping : Mapping,
      value : Any )
    extends WhereNode

  object Filter {
    trait Kind
    object Kind {
      case object Equals extends Kind
      case object NotEquals extends Kind
      case object Bigger extends Kind
      case object BiggerIncluding extends Kind
      case object Smaller extends Kind
      case object SmallerIncluding extends Kind
      case object In extends Kind
      case object Contains extends Kind
      case object Like extends Kind
      case object Regex extends Kind

    }
  }


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
