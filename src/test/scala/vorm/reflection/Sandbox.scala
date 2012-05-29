package vorm.reflection

import vorm.mirrorQuirks._

object Sandbox extends App {

  object Container {
    case class Genre(name: String)

    val t = tpe[Seq[Set[Genre]]]
  }


  val t1 = tpe[Seq[Set[Container.Genre]]]
  val t2 = Container.t



//  println(t1.toString)
//  println(t2)
//
  println(t1 == t2)

}
