package sorm.core

import shapeless._

package object members {

  // NOTE: Can't be a case class, be cause it mustn't be a Product.
  abstract class Member {
    type Value
    def persistedMixiner: api.PersistedMixiner[ Value ]
    def uniqueKeys: Set[Key] = Set.empty
    def nonUniqueKeys: Set[Key] = Set.empty
  }

  type Key = Seq[Symbol]

  trait API {
    protected val members: Members

    protected trait Members {
      type HList <: shapeless.HList
      val hlist: HList
    }
    protected object Members {
      def fromTuple
        [ tuple <: Product ]
        ( tuple: tuple )
        ( implicit generic: Generic[ tuple ]{ type Repr <: HList } )
        =
        new Members {
          type HList = generic.Repr
          override val hlist = generic.to(tuple)
        }
    }

    @annotation.implicitNotFound("The type `${a}` is not registered as a member")
    trait MemberResolver[ a ]{ def apply: Member }
    object MemberResolver {
      implicit def default
        [ a ]
        ( implicit selector: util.typeLevel.hlist.Selector[ members.HList, Member{ type Value <: a } ] )
        =
        new MemberResolver[ a ]{ def apply = selector.apply(members.hlist) }
    }

    def member[ a ] = new Member {
      type Value = a
      def persistedMixiner = ???
    }
  }

}
