package vorm.api

import vorm._
import reflection._
import structure._

sealed case class Entity
  [ T : TypeTag ]
  ( indexes       : Set[Seq[String]] = Set(),
    uniqueKeys    : Set[Seq[String]] = Set() )
  {
    //  Test validity of provided data:
    {
      ( indexes.view.flatten ++ uniqueKeys.view.flatten )
        .distinct
        .foreach{ p =>
          require( reflection.properties.containsKey(p),
                   "Inexistent property: `" + p + "`" )
        }
    }

    lazy val reflection
      = Reflection[T]
    def settings
      = EntitySettings(indexes, uniqueKeys)
  }