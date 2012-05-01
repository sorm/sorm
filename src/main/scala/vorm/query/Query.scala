package vorm.query

/**
 * This class may be used both to construct queries and to
 */
case class Query(
  filters: List[Filter] = Nil,
  orderings: List[Ordering] = Nil,
  limit: Limit = Limit(0, None)
)
