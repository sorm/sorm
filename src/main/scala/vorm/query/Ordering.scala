package vorm.query

case class Ordering(
  property: String,
  reverse: Boolean = false
)