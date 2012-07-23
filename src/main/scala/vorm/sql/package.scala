package vorm

package sql {

  case class Select (
    columns : List[Column],
    from    : From,
    joins   : List[Join]     = Nil,
    where   : Option[Where]  = None,
    order   : Option[Order]  = None,
    limit   : Option[Limit]  = None
  )

  case class Column (
    name    : String,
    table   : Option[String] = None
  )

  case class From (
    name    : String,
    alias   : Option[String] = None
  )

  /**
   * @param targetTable An identifier (name or alias) of the table to which the
   * join is applied
   * @param on A list of mappings of name of column of the table being joined to
   * the name of column of the target table
   * @param kind A type of join. `Left` by default
   */
  case class Join (
    name        : String,
    alias       : Option[String] = None,
    targetTable : String,
    on          : Seq[(String, String)],
    kind        : JoinKind = JoinKind.Left
  )

  sealed trait JoinKind
  object JoinKind {
    object Left  extends JoinKind
    object Right extends JoinKind
  }

  case class Where(
    condition: Condition
  )

  case class Order(

  ) {
    throw new NotImplementedError
  }

  case class Limit(

  ) {
    throw new NotImplementedError
  }

  trait Condition
  object Condition {
    case class Equal(column: String, value: Any) extends Condition
    case class NotEqual(column: String, value: Any) extends Condition
    case class More(column: String, value: Any) extends Condition
    case class MoreIncluding(column: String, value: Any) extends Condition
    case class Less(column: String, value: Any) extends Condition
    case class LessIncluding(column: String, value: Any) extends Condition
    case class Like(column: String, value: Any) extends Condition
    case class Regex(column: String, value: Any) extends Condition
    case class In(column: String, value: Any) extends Condition

    case class Or(left: Condition, right: Condition) extends Condition
    case class And(left: Condition, right: Condition) extends Condition
  }
}