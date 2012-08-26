package sorm.reflection

import sorm.extensions.Extensions._

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}

object Sandbox extends App {

  case class A ( a : Seq[Int] ) {
    def this(b:Int) = this(Seq(b))
    def this(a: Int, b: Int) = this(a :: b :: Nil)
    type B = Seq[String]
  }
  trait Pers


  Reflection[ReflectionSuite.Wrapper#InnerClass].signature.prettyString.trace()

}
