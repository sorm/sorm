package sorm.core.util

sealed trait HList {
  def ::[h](h: h): HList.Cons[h, this.type] = HList.Cons(h, this)
}
object HList {
  case class Cons[head, +tail <: HList](head: head, tail: tail) extends HList
  sealed trait Nil extends HList
  object Nil extends Nil

  @annotation.implicitNotFound(msg = "Element[${l}, ${v}] instance not found")
  trait Element[l, v] {
    def contains(l: l, v: v) = get(l) == v
    def get(l: l): v
  }
  implicit def headHasInstance[v, t <: HList] =
    new Element[Cons[v, t], v] {
      def get(l: Cons[v, t]) = l.head
    }
  implicit def tailHasInstance[v, h, t <: HList](implicit ins: Element[t, v]) =
    new Element[Cons[h, t], v] {
      def get(l: Cons[h, t]) = ins.get(l.tail)
    }

}
