package vorm.api

import vorm.query._

class QueryStream[T <: Persisted](connection: Connection, query: Query = Query()) extends Stream[T] {

  def filter(filter: Filter): this.type =
    new QueryStream[T](
      connection,
      query.copy(filters = filter :: query.filters)
    )

  def filterEquals(prop: String, value: Any): this.type =
    new QueryStream[T](
      connection,
      query.copy(filters = Filter.Equals(prop, value) :: query.filters)
    )

  def order(prop: String, reverse: Boolean = false): this.type =
    new QueryStream[T](
      connection,
      query.copy(orderings = Ordering(prop, reverse) :: query.orderings)
    )

  def offset(offset: Int): this.type =
    new QueryStream[T](
      connection,
      query.copy(limit = query.limit.copy(offset = offset))
    )

  def limit(amount: Int): this.type =
    new QueryStream[T](
      connection,
      query.copy(limit = query.limit.copy(amount = Some(amount)))
    )

  override def isEmpty: Boolean =
    throw new NotImplementedError

  override def head: T =
    throw new NotImplementedError

  override def tail: this.type =
    throw new NotImplementedError

  override protected def tailDefined: Boolean =
    throw new NotImplementedError
}
