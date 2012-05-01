package vorm.api

case class Limit(
  offset: Int = 0,
  amount: Option[Int] = None
)