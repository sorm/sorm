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
    = {
      val args
        = id ::
          reflected.reflection.constructorArguments
            .view
            .unzip._1
            .map{ reflected.propertyValue }
            .toList

      persistedClass(reflected.reflection)
        .getConstructors.head
        .newInstance(args.asInstanceOf[List[Object]]: _*)
        .asInstanceOf[T with Persisted]

    }

  def persisted
    [ T ]
    ( args : Map[String, Any],
      id : Long )
    : T with Persisted
    = {
      persistedClass(reflected.reflection)
    }




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