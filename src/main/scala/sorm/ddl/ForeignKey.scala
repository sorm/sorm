package sorm.ddl

import sorm._
import extensions._
import ForeignKey._

sealed case class ForeignKey
  ( table     : String,
    bindings  : Seq[(String, String)],
    onDelete  : ReferenceOption = ReferenceOption.NoAction,
    onUpdate  : ReferenceOption = ReferenceOption.NoAction )
  {
    def ddl
      = "FOREIGN KEY\n" + 
        ( "( " + bindings.view.unzip._1.map{quote}.mkString(", ") + " )\n" +
          "REFERENCES " + quote(table) + "\n" +
          "( " + bindings.view.unzip._2.map{quote}.mkString(", ") + " )\n" +
          "ON DELETE " + onDelete.ddl + "\n" +
          "ON UPDATE " + onUpdate.ddl )
          .indent(2)
  }

object ForeignKey {

  sealed abstract class ReferenceOption
    ( val ddl : String )
  object ReferenceOption {
    case object Restrict    extends ReferenceOption( "RESTRICT" )
    case object Cascade     extends ReferenceOption( "CASCADE" )
    case object NoAction    extends ReferenceOption( "NO ACTION" )
    case object SetNull     extends ReferenceOption( "SET NULL" )
    case object SetDefault  extends ReferenceOption( "SET DEFAULT" )
  }

}