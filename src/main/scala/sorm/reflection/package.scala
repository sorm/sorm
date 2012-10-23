package sorm.reflection

import reflect.runtime.universe._
import sext._, embrace._
import reflect.runtime.currentMirror
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
        = try {
            c .getConstructors.head
              .newInstance(args.asInstanceOf[Seq[Object]]: _*)
              .asInstanceOf[T]
          } catch {
            case e : IllegalArgumentException =>
              throw new IllegalArgumentException(
                e.getMessage + ":\n" +
                "Incorrect values of parameter types:\n" +
                c.getConstructors.head.getParameterTypes.view
                  .zip(args)
                  .filter{ case (t, v) => !t.isAssignableFrom(v.getClass) }
                  .map{ case (t, v) => t -> (v.getClass -> v) }
                  .valueTreeString,
                e
              )
          }
    }

}
