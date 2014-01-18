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
 * Encodes the complete info at compile time.
 */
sealed trait TypePath[ root ]
object TypePath {
  class Root[ root ] extends TypePath[ root ]
  class Member[ root, parent <: TypePath[ root ], index <: shapeless.Nat ] extends TypePath[ root ]
}

trait ToType[ a ] {
  val toType: ru.Type
}
object ToType {
  implicit def i1
    [ root ]
    ( implicit rootTT: ru.TypeTag[root] )
    =
    new ToType[ TypePath.Root[ root ] ] {
      val toType = rootTT.tpe
    }
  implicit def i2
    [ root, parent <: TypePath[root], index <: shapeless.Nat ]
    ( implicit parentToType: ToType[parent], indexToInt: shapeless.ops.nat.ToInt[index] )
    =
    new ToType[ TypePath.Member[ root, parent, index ] ] {
      val toType = {
        val index = indexToInt.apply()
        val parentType = parentToType.toType

        if( parentType <:< ru.typeOf[Product] ) {
          val properties = parentType.members.toStream.filter(_.isTerm).filter(!_.isMethod).reverse
          properties.apply(index).asTerm.typeSignatureIn(parentType)
        } else {
          parentType.asInstanceOf[ru.TypeRef].args(index)
//          sys.error("Unsupported type for access of member by index: " ++ parentType.typeSymbol.fullName)
        }
      }
    }
}

//trait MemberTypeAt[ a, index <: shapeless.Nat ] {
//  type Member
//}
//object MemberTypeAt {
//  implicit def i1[ member ] = new MemberTypeAt[ Product1[member], shapeless._0 ] {
//    type Member = member
//  }
//  implicit def i2[ member ] = new MemberTypeAt[ Product2[member, _], shapeless._0 ] {
//    type Member = member
//  }
//  implicit def i3[ member ] = new MemberTypeAt[ Product2[_, member], shapeless.nat._1 ] {
//    type Member = member
//  }
//}


// sealed trait KeyKind
// object KeyKind {
//   class Unique extends KeyKind
//   class NonUnique extends KeyKind
// }



}