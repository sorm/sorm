package vorm.persisted

import vorm._
import reflection._
import extensions._

trait Persisted {
  def id: Long
}
object Persisted {

  def apply
    [ T <: AnyRef : TypeTag ]
    ( instance: T,
      id: Long )
    : T with Persisted
    = apply( instance.reflected, id )

  def apply
    [ T ]
    ( reflected : Reflected,
      id : Long )
    : T with Persisted
    = apply( reflected.propertyValues, id, reflected.reflection)
        .asInstanceOf[T with Persisted]

  def apply
    [ T : TypeTag ]
    ( args : Map[String, Any],
      id : Long )
    : T with Persisted
    = apply( args, id, Reflection[T] )
        .asInstanceOf[T with Persisted]

  def apply
    ( args : Map[String, Any],
      id : Long,
      r : Reflection )
    : Persisted
    = PersistedClass(r)
        .instantiate(
          id +: r.constructorArguments.keysIterator.toStream.map{args}
        )

}