package vorm.abstractSql

import vorm._
import sql._
import extensions._

import AbstractSql._
import sql.Compositing._

object SqlComposition {

  def sql
    ( operator : Operator )
    : Sql.ComparisonOperator
    = operator match {
        case Equal          => Sql.Equal
        case NotEqual       => Sql.NotEqual
        case Larger         => Sql.Larger
        case LargerOrEqual  => Sql.LargerOrEqual
        case Smaller        => Sql.Smaller
        case SmallerOrEqual => Sql.SmallerOrEqual
        case Like           => Sql.Like
        case NotLike        => Sql.NotLike
        case Regexp         => Sql.Regexp
        case NotRegexp      => Sql.NotRegexp
        case In             => Sql.In
        case NotIn          => Sql.NotIn
      }

  /**
   * A helper that extracts all referred and intermediate table references
   */
  def allTables
    ( select : Select )
    : Seq[Table]
    = {
      lazy val references : Set[Table]
        = {
          def conditionTables ( condition : Condition ) : Seq[Table]
            = condition match {
                case And(l, r) =>
                  conditionTables(l) ++ conditionTables(r)
                case Or(l, r) =>
                  conditionTables(l) ++ conditionTables(r)
                case Comparison(t, _, _, _) =>
                  t +: Stream()
              }
          Set() ++
          select.expressions.view.collect{ case Column(n, t) => t } ++
          select.condition.toSeq.flatMap{conditionTables} ++
          select.havingCount.map{_.table}
        }

      lazy val tableParents : Map[Table, Seq[Table]]
        = references
            .zipBy{
              _.unfold{ _.parent.map{_.table}.map{t => t -> t } }
            }
            .toMap

      references
        .foldRight( Stream[Table]() ){ (t, acc) =>
          t +: tableParents(t) ++: acc
        }
        .reverse
        .distinct
    }

}
