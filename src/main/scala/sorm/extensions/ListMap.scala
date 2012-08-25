package sorm.extensions

object ListMap {
  def apply[A, B](elems: (A, B)*) =
    new ListMap(Map(elems: _*), List(elems: _*))
}
class ListMap[A, B](
  map: Map[A, B],
  protected val list: List[(A, B)]
) extends Map[A, B] {
  def get(key: A) =
    map.get(key)
  def iterator =
    list.iterator
  def +[B1 >: B](kv: (A, B1)) =
    new ListMap(
      map + kv,
      kv :: list
    )
  def -(key: A) =
    new ListMap(
      map - key,
      list filter (_._1 != key)
    )
  override def hashCode() =
    list.hashCode
  override def equals(that: Any) =
    that match {
      case that: ListMap[A, B] =>
        list.equals(that.list)
      case _ =>
        super.equals(that)
    }
}
