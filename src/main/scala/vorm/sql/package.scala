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
  trait ConditionObject

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

    trait Condition extends Clause
    
    case class Equals
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition

  }

  case class Value
    ( value : Any )
    extends ConditionObject


  case class GroupBy
    ( what : Seq[Column] )


}