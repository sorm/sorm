package vorm.structure

sealed trait Membership
  [ ParentT ]
  {
    def parent: ParentT
  }

object Membership {

  sealed case class Named
    [ ParentT ]
    ( name : String , parent : ParentT )
    extends Membership
  
  sealed case class Indexed
    [ ParentT ]
    ( index : Int , parent : ParentT )
    extends Membership

  sealed case class Single
    [ ParentT ]
    ( name : String , parent : ParentT )
    extends Membership

}
