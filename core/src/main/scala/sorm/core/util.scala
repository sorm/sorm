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


}
