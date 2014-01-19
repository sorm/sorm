package sorm
package object core {

import scala.reflect.runtime.{universe => ru}


def bug ( m : String ) = sys.error("A SORM bug appeared. Please, report the following message to the maintainers: " + m)


/**
 * A reference from a context type to its subfield. E.g., `context.field` or
 * `context.field.field` and so on.
 *
 * @tparam source A type of the context object.
 * @tparam target A type of the referred end-value.
 * @param contextType A type object of the context object.
 * @param subFieldSymbols A list of symbols representing the chain of subfields.
 *
 * @example
 *   For example, a reference `a.b.c` should be represented as follows:
 *   {{{
 *   SubRef
 *     [ <Type of `a`>, <Type of `c`> ]
 *     ( <rep of type of `a`>, List( <Symbol of field `b` of type of `a`>,
 *                                   <Symbol of field `c` of type of `a.b`> ) )
 *   }}}
 */
case class FieldRef
  [ source, target ]
  ( contextType : ru.Type,
    subFieldSymbols : List[ Symbol ] )

sealed trait FieldRefs
  [ source, targets ]

object FieldRefs {
  case class FieldRefsValue
    [ source, targetsHead, targetsTail ]
    ( subRef : FieldRef[ source, targetsHead ],
      tail : FieldRefs[ source, targetsTail ] )
    extends FieldRefs[ source, (targetsHead, targetsTail) ]

  case class FieldRefsNil[ a ] extends FieldRefs[ a, Unit ]
}


// ----------------- the modern way

/**
 * A reference to a specific member from a root type.
 * Encodes a complete info at type-level.
 */
sealed trait TypePath[ root ]
object TypePath {
  final class Root[ root ] extends TypePath[ root ]
  final class Generic[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
  final class Property[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
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



}