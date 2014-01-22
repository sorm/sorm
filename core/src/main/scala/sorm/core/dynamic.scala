package sorm.core
package object dynamic {

import scala.reflect.runtime.{universe => ru}

sealed trait Member
object Member {
  case class Root( t: ru.Type ) extends Member
  case class Generic( parent: Member, index: Int ) extends Member
  case class Property( parent: Member, name: String ) extends Member
}

def memberType( member: Member ): ru.Type = {
  import Member._
  member match {
    case Root(t) => t
    case Generic(parent, index) => util.reflection.generic(memberType(parent), index)
    case Property(parent, name) =>
      val parentType = memberType(parent)
      parentType.member(ru.newTermName(name)).typeSignatureIn(parentType)
  }
}

trait MemberResolver[ a ] {
  def member: Member
}
object MemberResolver {
}


//  ------------------ OR GRANULARLY ----------------

sealed trait TypePath
object TypePath {
  case object Root extends TypePath
  case class Member( parent: TypePath, ref: ChildRef ) extends TypePath
}

// TODO: simply replace with an Int
sealed trait ChildRef
object ChildRef {
  case class ByName( name: String ) extends ChildRef
  case class ByIndex( index: Int ) extends ChildRef
}

trait ChildRefResolver[-a] {
  def childRef: ChildRef
}
object ChildRefResolver {
  implicit def propertyInstance
    [ root, parent <: static.TypePath[root], index <: shapeless.Nat ]
    ( implicit
        parentTypeResolver: static.TypeResolver[parent],
        indexToInt: shapeless.ops.nat.ToInt[index] )
    =
    new ChildRefResolver[static.TypePath.Property[root, parent, index]] {
      def childRef = {
        val name = {
          val parentType = parentTypeResolver.head
          val index = indexToInt.apply()
          val property = util.reflection.properties(parentType).apply(index)
          util.reflection.name(property)
        }
        ChildRef.ByName(name)
      }
    }
  implicit def genericInstance
    [ root, parent <: static.TypePath[root], index <: shapeless.Nat ]
    ( implicit indexToInt: shapeless.ops.nat.ToInt[index] )
    =
    new ChildRefResolver[static.TypePath.Generic[root, parent, index]] {
      def childRef = {
        val index = indexToInt.apply()
        ChildRef.ByIndex(index)
      }
    }

}

object helpers {
  def childType( t: ru.Type, ref: ChildRef ) = {
    import ChildRef._
    ref match {
      case ByName(name) => t.member(ru.newTermName(name)).typeSignatureIn(t).asInstanceOf[ru.NullaryMethodType].resultType
      case ByIndex(index) => util.reflection.generic(t, index)
    }
  }
}

}