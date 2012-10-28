package sorm.reflection

import sorm._
import sext._, embrace._

//  or InstanceReflection
class Reflected
  ( val instance : Any,
    val reflection : Reflection )
  {

    def propertyValues
      : Map[String, Any]
      = reflection.properties.view.unzip._1.zipBy(propertyValue).toMap

    def propertyValue
      ( name: String )
      : Any
      = reflection.propertyValue(name, instance.asInstanceOf[AnyRef])

    def methodResult
      ( name: String,
        args: List[Any] = Nil )
      : Any
      = throw new NotImplementedError

  }
