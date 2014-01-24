package sorm.core

import collection._, generic._

package object util {

  def memo [ a, z ] ( f : a => z ) = {
     // a WeakHashMap will release cache members if memory tightens
     val cache = new collection.mutable.WeakHashMap[a, z]
     x : a => synchronized { cache.getOrElseUpdate( x, f(x) ) }
  }

  object typeLevel {
    /**
     * Basically a bool with type-level values.
     */
    sealed trait Bool {
      val toBoolean: Boolean
    }
    object Bool {
      sealed trait True extends Bool
      sealed trait False extends Bool
      case object True extends True { val toBoolean = true }
      case object False extends False { val toBoolean = false }
    }
  }

  object reflection {
    import reflect.runtime.universe._
    def isTuple(t: Type) =
      t <:< typeOf[Tuple1[_]] ||
      t <:< typeOf[Tuple2[_, _]] ||
      t <:< typeOf[Tuple3[_, _, _]] ||
      t <:< typeOf[Tuple4[_, _, _, _]] ||
      t <:< typeOf[Tuple5[_, _, _, _, _]] ||
      t <:< typeOf[Tuple6[_, _, _, _, _, _]] ||
      t <:< typeOf[Tuple7[_, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple8[_, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple9[_, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple10[_, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]] ||
      t <:< typeOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    def generic(t: Type, i: Int) = t.asInstanceOf[TypeRef].args(i)
    def name(s: Symbol) = s.name.decoded.trim
    def properties(t: Type) = t.members.toStream.filter(_.isTerm).filter(!_.isMethod).reverse
  }

  implicit class IterableLikeUtil[+A, +Repr]( val base: IterableLike[A, Repr] ) extends AnyVal {
    private def repr: Repr = base.asInstanceOf[Repr]
    def zipAllLazy[B, A1 >: A, That](that: GenIterable[B], thisElem: =>A1, thatElem: =>B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(repr)
      val these = base.iterator
      val those = that.iterator
      while (these.hasNext && those.hasNext)
        b += ((these.next, those.next))
      while (these.hasNext) {
        b += ((these.next, thatElem))
      }
      while (those.hasNext) {
        b += ((thisElem, those.next))
      }
      b.result
    }
  }

}
