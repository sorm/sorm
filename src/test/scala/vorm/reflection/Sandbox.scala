package vorm.reflection

import vorm._
import mirrorQuirks._
import extensions._

object Sandbox extends App {

  tpe[TypeTest.Artist]
    .instance(Map("id" -> "id1", "name" -> "name1", "genres" -> Set(), "tags" -> Set("tag1")))
    .println()

}
