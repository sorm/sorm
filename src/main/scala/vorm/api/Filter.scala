package vorm.api


trait Filter

object Filter {

  case class Equals(property: String, value: Any) extends Filter

}
