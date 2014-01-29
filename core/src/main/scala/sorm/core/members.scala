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
    protected val members: shapeless.HList

    def membersFromTuple
      [ tuple <: Product ]
      ( tuple: tuple )
      ( implicit generic: Generic[ tuple ]{ type Repr <: HList } )
      = generic.to(tuple)

    @annotation.implicitNotFound("The type `${a}` is not registered as a member")
    protected trait MemberResolver[ a ]{ def apply: Member }
    protected object MemberResolver {
      implicit def selector
        [ a ]
        ( implicit selector: util.typeLevel.hlist.Selector[ members.type, Member{ type Value <: a } ] )
        =
        new MemberResolver[ a ]{ def apply = selector.apply(members) }
    }

    def member[ a ] = new Member {
      type Value = a
      def persistedMixiner = ???
    }

  }
  object API {
    implicit def memberResolver
      [ api <: API, a ]
      ( implicit resolver: api#MemberResolver[ a ] )
      = new MemberResolver[ api, a ]{ def apply( api: api ) = resolver.apply }
  }
  @annotation.implicitNotFound("The type `${a}` is not registered as a member")
  trait MemberResolver[ api <: API, a ]{
    def apply( api: api ): Member
  }

}
