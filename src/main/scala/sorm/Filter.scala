package sorm

sealed trait Filter
object Filter {

  case class Or ( l : Filter, r : Filter ) extends Filter
  case class And ( l : Filter, r : Filter ) extends Filter

  case class Equal ( p : String, v : Any ) extends Filter
  case class NotEqual ( p : String, v : Any ) extends Filter
  case class Larger ( p : String, v : Any ) extends Filter
  case class LargerOrEqual ( p : String, v : Any ) extends Filter
  case class Smaller ( p : String, v : Any ) extends Filter
  case class SmallerOrEqual ( p : String, v : Any ) extends Filter
  case class Like ( p : String, v : Any ) extends Filter
  case class NotLike ( p : String, v : Any ) extends Filter
  case class Regex ( p : String, v : Any ) extends Filter
  case class NotRegex ( p : String, v : Any ) extends Filter
  case class In ( p : String, v : Any ) extends Filter
  case class NotIn ( p : String, v : Any ) extends Filter
  case class Contains ( p : String, v : Any ) extends Filter
  case class NotContains ( p : String, v : Any ) extends Filter
  /**
   * Makes part of a collection
   */
  case class Constitutes ( p : String, v : Any ) extends Filter
  case class NotConstitutes ( p : String, v : Any ) extends Filter
  /**
   * Includes a collection
   */
  case class Includes ( p : String, v : Any ) extends Filter
  case class NotIncludes ( p : String, v : Any ) extends Filter


}
