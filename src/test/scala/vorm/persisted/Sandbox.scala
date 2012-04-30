package vorm.persisted

import PersistedEnabler._

object Sandbox extends App {

  type S = reflect.api.Universe#Symbol

  case class Person(first: String, last: String)
  val p = toPersisted(Person("hello", "world"), 42)
  println(p.first)
  println(p.last)
  println(p.id)

}


