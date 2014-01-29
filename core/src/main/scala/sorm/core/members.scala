package sorm.core

import shapeless._

package object members {

  // NOTE: Not a case class, so that it isn't a Product
  @annotation.implicitNotFound(msg = "A Member[${a}] instance is not declared")
  class Member[a](persistedMixiner: api.PersistedMixiner[a], uniqueKeys: Set[Key], nonUniqueKeys: Set[Key])

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

    trait MemberResolver[ a ]{ def apply: Member[ a ] }
    object MemberResolver {
      implicit def default
        [ a ]
        ( implicit selector: ops.hlist.Selector[ members.HList, Member[ a ] ] )
        =
        new MemberResolver[ a ]{ def apply = selector.apply(members.hlist) }

    }
  }

}
