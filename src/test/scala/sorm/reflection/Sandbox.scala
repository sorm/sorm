package sorm.reflection

import sorm.extensions.Extensions._

import reflect.runtime.universe._

object Sandbox extends App {

  case class A ( a : Seq[Int] ) {
    def this(b:Int) = this(Seq(b))
    def this(a: Int, b: Int) = this(a :: b :: Nil)
    type B = Seq[String]
  }
  trait Pers

  Reflection[A].signature.prettyString.trace()

}
