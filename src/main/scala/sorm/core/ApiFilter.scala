package sorm.core

/**
 * A filter declaration which has no knowledge of mapping. For public API
 */
object ApiFilter {

  sealed trait Filter
  case class Equal ( p : String, v : Any ) extends Filter
  case class NotEqual ( p : String, v : Any ) extends Filter
  case class Larger ( p : String, v : Any ) extends Filter
  case class LargerOrEqual ( p : String, v : Any ) extends Filter
  case class Smaller ( p : String, v : Any ) extends Filter
  case class SmallerOrEqual ( p : String, v : Any ) extends Filter
  case class Or ( l : Filter, r : Filter ) extends Filter
  case class And ( l : Filter, r : Filter ) extends Filter


}
