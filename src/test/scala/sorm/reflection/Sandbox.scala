package sorm.reflection

import sorm.extensions.Extensions._

import reflect.runtime.universe._

object Sandbox extends App {

  case class A ( a : Seq[Int] ) {
    def this(b:Int) = this(Seq(b))
    def this(a: Int, b: Int) = this(a :: b :: Nil)
  }
  trait Pers

  Reflection[A].constructorArguments.prettyString.trace()

  val a = Reflection[A].members

  a.prettyString.trace()

}
