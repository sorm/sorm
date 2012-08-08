package vorm

import vorm._
import reflection._
import extensions._

package object persisted {


  def persisted
    [ T <: AnyRef : TypeTag ]
    ( instance: T, 
      id: Long )
    : T with Persisted 
    = persisted( instance.reflected, id )

  def persisted
    [ T ]
    ( reflected : Reflected,
      id : Long )
    : T with Persisted
    = persisted( reflected.propertyValues, id, reflected.reflection)
        .asInstanceOf[T with Persisted]

  def persisted
    [ T : TypeTag ]
    ( args : Map[String, Any],
      id : Long )
    : T with Persisted
    = persisted( args, id, Reflection[T] )
        .asInstanceOf[T with Persisted]

  def persisted
    ( args : Map[String, Any],
      id : Long,
      r : Reflection )
    : Persisted
    = persistedClass(r)
        .instantiate(
          id +: r.constructorArguments.keysIterator.toStream.map{args}
        )



  private val persistedClass0
    = new collection.mutable.HashMap[Reflection, Class[_ <: Persisted]] {
        override def default
          ( k : Reflection )
          = {
            val v = ClassCreator.createClass(k)
            update(k, v)
            v
          }
      }

  def persistedClass
    ( r : Reflection )
    = persistedClass0(r.mixinBasis)

}