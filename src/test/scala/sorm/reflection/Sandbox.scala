package sorm.reflection

import sorm.extensions.Extensions._

object Sandbox extends App {

  case class A ( a : Seq[Int] )

  Reflection[A].properties.prettyString.trace()

}
