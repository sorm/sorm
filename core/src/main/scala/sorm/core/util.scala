package sorm.core; package object util {

  def memo [ a, z ] ( f : a => z ) = {
     // a WeakHashMap will release cache members if memory tightens
     val cache = new collection.mutable.WeakHashMap[a, z]
     x : a => synchronized { cache.getOrElseUpdate( x, f(x) ) }
  }
  
}
