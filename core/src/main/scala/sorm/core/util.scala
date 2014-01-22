package sorm.core; package object util {

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
      case class True extends Bool { val toBoolean = true }
      case class False extends Bool { val toBoolean = false }
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
  }

}
