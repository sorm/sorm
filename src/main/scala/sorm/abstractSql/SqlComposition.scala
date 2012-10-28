package sorm.abstractSql

import sorm._
import sql._
import sext._, embrace._

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
                case _ =>
                  Stream()
              }
          Set() ++
          select.expressions.view.collect{ case Column(n, t) => t } ++
          select.condition.toSeq.flatMap{conditionTables} ++
          select.groupBy.map{_.table} ++
          select.having.map{_.table} ++
          select.order.map{_.table}
        }

      def tableParents ( t : Table ) : Seq[Table]
        = t.unfold{ _.parent.map{_.table}.map{t => t -> t } }

      references
        .foldRight( Stream[Table]() ){ (t, acc) =>
          t +: tableParents(t) ++: acc
        }
        .reverse
        .distinct
    }

}
