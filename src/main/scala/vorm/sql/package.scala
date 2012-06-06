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
    table   : Option[String] = None,
    alias   : Option[String] = None
  )

  case class From (
    table   : Table
  )

  case class Table (
    name    : String,
    alias   : Option[String] = None
  )

  /**
   * @param table A declaration of the table being joined to the target table
   * @param targetTable An identifier (name or alias) of the table to which the join
   *                    is performed
   * @param mappings A list of mappings of name of column of the table being joined to
   *                 the name of column of the target table
   * @param t A type of join. `Left` by default
   */
  case class Join (
    table       : Table,
    targetTable : String,
    mappings    : List[(String, String)],
    t           : JoinType = JoinType.Left
  )

  private final class JoinType
  object JoinType {
    val Left, Right = new JoinType
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