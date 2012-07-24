package vorm.select

import vorm._
import extensions._
import query._
import structure._
import reflection._


trait SelectWhat
case class Select
  ( what : Seq[SelectWhat],
    joins : Seq[Join],
    where : Option[Clause],
    groupBy : Option[GroupBy],
    having : Option[Clause] )
  extends FromWhat with JoinWhat

case class Table
  ( name : String )
  extends FromWhat with JoinWhat

case class From
  ( what : FromWhat,
    as : Option[String] = None )

trait FromWhat

case class Join
  ( what : JoinWhat,
    as : Option[String],
    to : String,
    on : Seq[(String, String)] = Nil,
    kind : JoinKind = JoinKind.Left )

trait JoinWhat

sealed trait JoinKind
object JoinKind {
  object Left  extends JoinKind
  object Right extends JoinKind
  object Inner extends JoinKind
  object Outer extends JoinKind
}


case class Column
  ( name : String,
    table : Option[String] )
  extends SelectWhat with ConditionObject

case class Count
  ( what : Seq[Column],
    distinct : Boolean = false )
  extends ConditionObject



trait Clause

case class Composite
  ( left : Clause, 
    relation : CompositeRelation,
    right : Clause )
  extends Clause

trait CompositeRelation
object CompositeRelation {
  case object And extends CompositeRelation
  case object Or extends CompositeRelation
}

case class Condition
  ( left : Either[ConditionObject, Any],
    relation : ConditionRelation,
    right : Either[ConditionObject, Any] )
  extends Clause

trait ConditionObject

trait ConditionRelation
object ConditionRelation {
  case object Equal extends ConditionRelation
  case object NotEqual extends ConditionRelation
  case object Larger extends ConditionRelation
  case object LargerIncluding extends ConditionRelation
  case object Smaller extends ConditionRelation
  case object SmallerIncluding extends ConditionRelation
  case object Contains extends ConditionRelation
  case object In extends ConditionRelation
}

case class GroupBy
  ( what : Seq[Column] )