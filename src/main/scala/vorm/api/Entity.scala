package vorm.api

import vorm._
import reflection._
import structure._
import extensions._

sealed case class Entity
  [ T : TypeTag ]
  ( indexes       : Set[Seq[String]] = Set(),
    uniqueKeys    : Set[Seq[String]] = Set() )
  {

    lazy val reflection
      = Reflection[T]
    def settings
      = EntitySettings(indexes, uniqueKeys)


    //  Validate input:
    {
      {
        def allDescendants
          ( r : Reflection )
          : Stream[Reflection]
          = ( r.properties.values.view ++ r.generics.view )
              .flatMap{r => r +: allDescendants(r)}
              .toStream

        allDescendants(reflection)
          .filter{_ inheritsFrom Reflection[Option[_]]}
          .foreach{ r =>
            require( !r.generics(0).inheritsFrom(Reflection[Option[_]]),
                     "Type signatures with `Option` being directly nested in another `Option`, i.e. `Option[Option[_]]` are not allowed" )
          }
      }

      ( indexes.view ++ uniqueKeys.view )
        .flatten
        .foreach{ p =>
          require( reflection.properties.contains(p),
                   "Inexistent property: `" + p + "`" )
        }

      ( indexes.view ++ uniqueKeys.view )
        .foreach{ ps =>
          require( ps.view.distinct.size == ps.size,
                   "Not a distinct properties list: `" + ps.mkString(", ") + "`" )
        }
    }
  }