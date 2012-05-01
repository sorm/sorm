package vorm.query

trait Filter


object Filter {

  case class Equals(property: String, value: Any) extends Filter
  case class NotEquals(property: String, value: Any) extends Filter

  //  case class More(property: String, value: Any) extends Filter
  //  case class MoreIncluding(property: String, value: Any) extends Filter
  //  case class Less(property: String, value: Any) extends Filter
  //  case class LessIncluding(property: String, value: Any) extends Filter
  //  case class Like(property: String, value: Any) extends Filter
  //  case class Regex(property: String, value: Any) extends Filter
  //  case class In(property: String, value: Any) extends Filter

  case class Or(left: Filter, right: Filter) extends Filter
  case class And(left: Filter, right: Filter) extends Filter

}
