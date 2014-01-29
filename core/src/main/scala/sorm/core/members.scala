package sorm.core

import shapeless._

package object members {

  // NOTE: Not a case class, so that it isn't a Product
  @annotation.implicitNotFound(msg = "A Member[${a}] instance is not declared")
  class Member[a](persistedMixiner: api.PersistedMixiner[a], uniqueKeys: Set[Key], nonUniqueKeys: Set[Key])

  type Key = Seq[Symbol]

  trait MemberResolver[ container <: Container, a ]{
    def apply( container: container ): Member[ a ]
  }

  trait Container {
    type MembersHList <: shapeless.HList
    protected val membersHList: MembersHList
  }
  object Container {
    implicit def memberResolver
      [ container <: Container, a ]
      ( implicit selector: ops.hlist.Selector[ container#MembersHList, Member[ a ] ] )
      =
      new MemberResolver[ container, a ]{
        def apply( container: container ) = selector.apply( container.membersHList )
      }
  }

  abstract class ContainerFromTuple
    [ tuple <: Product, hlist <: HList ]
    ( tuple: tuple )
    ( implicit tupleGeneric: Generic.Aux[tuple, hlist] )
    extends Container
    {
      type MembersHList = hlist
      protected val membersHList: MembersHList = tupleGeneric.to(tuple)
    }

  abstract class ContainerFromHList
    [ hlist <: HList ]
    ( hlist: hlist )
    extends Container
    {
      type MembersHList = hlist
      protected val membersHList: MembersHList = hlist
    }

}
