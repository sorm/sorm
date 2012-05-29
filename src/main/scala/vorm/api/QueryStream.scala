package vorm.api

import vorm.query._

class QueryStream[T <: AnyRef](connection: Connection, query: Query = Query()) extends Stream[T with Persisted] {

  def filter(filter: Filter) =
    new QueryStream[T](
      connection,
      query.copy(filters = filter :: query.filters)
    )

  def filterEquals(prop: String, value: Any) =
    new QueryStream[T](
      connection,
      query.copy(filters = Filter.Equals(prop, value) :: query.filters)
    )

  def order(prop: String, reverse: Boolean = false) =
    new QueryStream[T](
      connection,
      query.copy(orderings = Ordering(prop, reverse) :: query.orderings)
    )

  def offset(offset: Int) =
    new QueryStream[T](
      connection,
      query.copy(limit = query.limit.copy(offset = offset))
    )

  def limit(amount: Int) =
    new QueryStream[T](
      connection,
      query.copy(limit = query.limit.copy(amount = Some(amount)))
    )

  override def isEmpty: Boolean =
    throw new NotImplementedError

  override def head: T with Persisted =
    throw new NotImplementedError

  override def tail: QueryStream[T with Persisted] =
    throw new NotImplementedError

  override protected def tailDefined: Boolean =
    throw new NotImplementedError

  override def length: Int =
    throw new NotImplementedError
}
