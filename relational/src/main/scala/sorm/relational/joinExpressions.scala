/**
 * A Select-query template AST, which abstracts away from table aliases and 
 * from-join management, 
 * using table references which represent specific tables in a query and 
 * information on how they should be joined.
 */
package sorm.relational.joinExpressions

object templates {

  case class Column ( name: String, from: From )

  sealed trait From
  object From {
    case class Root
      ( name: String )
      extends From
    case class Join
      ( name: String, parent: From, bindings: Seq[(String, String)] )
      extends From
  }

  sealed trait Where
  object Where {
    case class Fork ( left: Where, right: Where, or: Boolean ) extends Where
    case class Comparison
      ( column: Column, operator: Operator, value: Expression, negative: Boolean )
      extends Where
  }

  sealed trait Operator
  object Operator {
    case object Equal extends Operator
    case object Larger extends Operator
    case object Smaller extends Operator
    case object Like extends Operator
    case object Regexp extends Operator
    case object In extends Operator
  }

  sealed trait Expression
  object Expression {
    case object Placeholder extends Expression
    // case class Select( select: sorm.relational.queryJoinTemplates.Select ) extends Expression
  }

  // case class Select
  //   ( what: What,
  //     where: Where,
  //     having: Having,
  //     groupBy: GroupBy,
  //     limit: Option[Int] = Nothing,
  //     offset: Int = 0 )

  // type GroupBy =

}

/**
 * Completely runtime stuff.
 */
object functions {

  import sorm.relational._
  import templates._
  import sorm.core._
  import reflect.runtime.{universe => ru}
  import rules._

  def column( mapping: Mapping ): Option[Column] = {
    for {
      name <- mapping.memberNameBasis
      from <- mapping.parent.flatMap(this.from)
    }
    yield Column(name, from)
  }

  def from( mapping: Mapping ): Option[From] = {
    mapping.parent match {
      case None => mapping.tableName.map(From.Root)
      case Some(parentMapping) => {
        def bindingsFromParent = mapping.foreignKeyForParent.map(_.bindings.map(_.swap))
        def bindingsToParent = mapping.foreignKeyToParent.map(_.bindings)
        for {
          parent <- from(parentMapping)
          name <- mapping.tableName
          bindings <- bindingsFromParent.orElse(bindingsToParent)
        }
        yield From.Join(name, parent, bindings)
      }
    }
  }


}
