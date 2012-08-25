package sorm.extensions

import collection.immutable.Queue

object OrderedMap {
  def apply[A, B](elems: (A, B)*) =
    new OrderedMap(Map(elems: _*), Queue(elems: _*))
}
/**
 * An ordered map implementation that should perform effectively on all operations except
 * removal, where it performs linearly.
 */
class OrderedMap[A, B](
  map: Map[A, B],
  protected val queue: Queue[(A, B)]
) extends Map[A, B] {
  def get(key: A) =
    map.get(key)
  def iterator =
    queue.iterator
  def +[B1 >: B](kv: (A, B1)) =
    new OrderedMap(
      map + kv,
      queue enqueue kv
    )
  def -(key: A) =
    new OrderedMap(
      map - key,
      queue filter (_._1 != key)
    )
  override def hashCode() =
    queue.hashCode
  override def equals(that: Any) =
    that match {
      case that: OrderedMap[A, B] =>
        queue.equals(that.queue)
      case _ =>
        super.equals(that)
    }
}
