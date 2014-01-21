package sorm.core.static

import sorm.core._
import scala.reflect.runtime.{universe => ru}

/**
 * A reference to a specific member from a root type.
 * Encodes a complete info at type-level.
 */
sealed trait TypePath[ root ]
object TypePath {
  sealed trait Root[ root ] extends TypePath[ root ]
  sealed trait Generic[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
  sealed trait Property[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
}

trait TypeResolver[ a ] {
  val head: ru.Type
  /**
   * All the ancestors up to the root.
   */
  val tail: Seq[ru.Type]
  lazy val seq = head +: tail.toStream
}
object TypeResolver {
  implicit def rootTypeResolver
    [ root ]
    ( implicit rootTT: ru.TypeTag[root] )
    =
    new TypeResolver[ TypePath.Root[ root ] ] {
      val head = rootTT.tpe
      val tail = Nil
    }
  implicit def genericTypeResolver
    [ root, parent <: TypePath[root], index <: shapeless.Nat ]
    ( implicit parentTypeResolver: TypeResolver[ parent ], indexToInt: shapeless.ops.nat.ToInt[ index ] )
    =
    new TypeResolver[ TypePath.Generic[ root, parent, index ] ] {
      val head = {
        val index = indexToInt.apply()
        val parentType = parentTypeResolver.head
        util.reflection.generic(parentType, index)
      }
      lazy val tail = parentTypeResolver.head +: parentTypeResolver.tail
    }
  implicit def propertyTypeResolver
    [ root, parent <: TypePath[root], index <: shapeless.Nat ]
    ( implicit parentTypeResolver: TypeResolver[ parent ], indexToInt: shapeless.ops.nat.ToInt[ index ] )
    =
    new TypeResolver[ TypePath.Property[ root, parent, index ] ] {
      val head = {
        val index = indexToInt.apply()
        val parentType = parentTypeResolver.head
        val properties = parentType.members.toStream.filter(_.isTerm).filter(!_.isMethod).reverse
        properties.apply(index).asTerm.typeSignatureIn(parentType)
      }
      lazy val tail = parentTypeResolver.head +: parentTypeResolver.tail
    }
}


// sealed trait KeyKind
// object KeyKind {
//   class Unique extends KeyKind
//   class NonUnique extends KeyKind
// }

