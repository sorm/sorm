package vorm.query

import vorm.reflection._

case class Query(
  t: Type,
  filters: List[Filter] = Nil,
  orderings: List[Ordering] = Nil,
  limit: Limit = Limit(0, None)
)
