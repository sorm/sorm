package vorm

package sql {

  case class Select
    ( what : Seq[SelectObject],
      from : From,
      join : Seq[Join] = Nil,
      where : Option[Clause] = None,
      groupBy : Seq[Column] = Nil,
      having : Option[Clause] = None )
    extends FromObject with JoinObject

  trait SelectObject

  case class Table
    ( name : String )
    extends FromObject with JoinObject

  case class From
    ( what : FromObject,
      as : Option[String] = None )

  trait FromObject

  case class Join
    ( what : JoinObject,
      as : Option[String],
      on : Seq[(Column, Column)] = Nil,
      kind : JoinKind = JoinKind.Left )

  trait JoinObject

  trait JoinKind
  object JoinKind {
    object Left  extends JoinKind
    object Right extends JoinKind
    object Inner extends JoinKind
    object Outer extends JoinKind
  }


  case class Column
    ( name : String,
      table : Option[String] )
    extends SelectObject with ConditionObject

  case class Count
    ( what : Seq[Column],
      distinct : Boolean = false )
    extends ConditionObject



  trait Clause

  object Clause {
    
    trait Composite extends Clause {
      def left : Clause
      def right : Clause
    }

    case class And
      ( left : Clause,
        right : Clause )
      extends Composite

    case class Or
      ( left : Clause,
        right : Clause )
      extends Composite

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


}