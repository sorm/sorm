package vorm.ddl

import ForeignKey._

sealed case class ForeignKey
  ( table     : String,
    bindings  : Seq[(String, String)],
    onDelete  : ReferenceOption = ReferenceOption.NoAction,
    onUpdate  : ReferenceOption = ReferenceOption.NoAction )

object ForeignKey {

  sealed abstract class ReferenceOption
    ( val ddl : String )
  object ReferenceOption {
    case object Restrict    extends ReferenceOption ("RESTRICT")
    case object Cascade     extends ReferenceOption ("CASCADE")
    case object NoAction    extends ReferenceOption ("NO ACTION")
    case object SetNull     extends ReferenceOption ("SET NULL")
    case object SetDefault  extends ReferenceOption ("SET DEFAULT")
  }

}