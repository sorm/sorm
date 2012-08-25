package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._
import ScalaApi._

object `package` {

  implicit class AnyReflected
    [ T : TypeTag ]
    ( any : T )
    {
      def reflected
        = new Reflected( any, Reflection( typeTag[T] ) )
    }

  implicit class ClassAdapter
    [ T ]
    ( c : Class[T] )
    {
      def instantiate
        ( args : Seq[Any] )
        : T
        = c .getConstructors.head
            .newInstance(args.asInstanceOf[Seq[Object]]: _*)
            .asInstanceOf[T]
    }

}
