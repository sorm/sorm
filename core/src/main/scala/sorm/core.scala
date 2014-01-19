package sorm; package object core {


import scala.reflect.runtime.{universe => ru}


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
  final class OptionItem[ root, parent <: TypePath[ root ] ] extends TypePath[ root ]
  final class SeqItem[ root, parent <: TypePath[ root ] ] extends TypePath[ root ]
  final class SetItem[ root, parent <: TypePath[ root ] ] extends TypePath[ root ]
  final class MapKey[ root, parent <: TypePath[ root ] ] extends TypePath[ root ]
  final class MapValue[ root, parent <: TypePath[ root ] ] extends TypePath[ root ]
  final class TupleMember[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
  final class CaseClassMember[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
}

trait ToType[ a ] {
  val toType: ru.Type
}
object ToType {
  implicit def rootToType
    [ root ]
    ( implicit rootTT: ru.TypeTag[root] )
    =
    new ToType[ TypePath.Root[ root ] ] {
      val toType = rootTT.tpe
    }
  implicit def optionToType
    [ root, parent <: TypePath[ root ] ]
    ( implicit parentToType: ToType[ parent ] )
    =
    new ToType[ TypePath.OptionItem[ root, parent ] ] {
      val toType = {
        val parentType = parentToType.toType
        util.reflection.generic(parentType, 0)
      }
    }
  implicit def tupleMemberToType
    [ root, parent <: TypePath[root], index <: shapeless.Nat ]
    ( implicit parentToType: ToType[ parent ], indexToInt: shapeless.ops.nat.ToInt[ index ] )
    =
    new ToType[ TypePath.TupleMember[ root, parent, index ] ] {
      val toType = {
        val index = indexToInt.apply()
        val parentType = parentToType.toType
        val properties = parentType.members.toStream.filter(_.isTerm).filter(!_.isMethod).reverse
        properties.apply(index).asTerm.typeSignatureIn(parentType)
      }
    }
  implicit def caseClassMemberToType
    [ root, parent <: TypePath[root], index <: shapeless.Nat ]
    ( implicit parentToType: ToType[ parent ], indexToInt: shapeless.ops.nat.ToInt[ index ] )
    =
    new ToType[ TypePath.CaseClassMember[ root, parent, index ] ] {
      val toType = {
        val index = indexToInt.apply()
        val parentType = parentToType.toType
        val properties = parentType.members.toStream.filter(_.isTerm).filter(!_.isMethod).reverse
        properties.apply(index).asTerm.typeSignatureIn(parentType)
      }
    }
}


// sealed trait KeyKind
// object KeyKind {
//   class Unique extends KeyKind
//   class NonUnique extends KeyKind
// }



}