package sorm.structure

import sorm._

object ReflectionValues {

  sealed trait Ref

  case class EnumRef
    ( name : String,
      values : Enumeration#Value )
    extends Ref

  case class EntityRef
    ( name : String,
      properties : Map[String, Ref] )
    extends Ref

  case class SeqRef
    ( item : Ref )
    extends Ref

  object Ref {
    def apply
      ( r : reflection.Reflection )
      = ReflectionKind(r) match {
          case ReflectionKind.Entity => ???
          case _ => ???
        }

  }


}
