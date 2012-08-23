package vorm.abstractSql

import vorm._
import sql._
import extensions._

import AbstractSql._
import sql.Composition._

object ToSql {

  def sql
    ( statement : Statement )
    : Sql.Statement
    = statement match {
        case Union(l, r) => 
          Sql.Union( sql(l), sql(r) )
        case Intersection(l, r) => 
          sql(l) narrow sql(r)
        case s : Select =>

          val tables = allTables(s)

          val aliases : Map[Table, String]
            = tables.view.zipWithIndex
                .map{ case(t, i) => t -> alias(i) }
                .toMap

          Sql.Select(
            what
              = s.expressions.view
                  .collect{ 
                    case Column(n, t) => 
                      Sql.Column(n, Some(aliases(t)))
                  },
            from
              = Sql.From(Sql.Table(tables.head.name), 
                         Some(aliases(tables.head))),
            join
              = tables.toStream.tail
                  .map{ t => 
                    Sql.Join(
                      Sql.Table(t.name),
                      Some(aliases(t)),
                      t.parent.get.bindings
                        .map{ case (l, r) =>
                          Sql.Column(l, Some(aliases(t))) ->
                          Sql.Column(r, Some(aliases(t.parent.get.table)))
                        }
                    )
                  },
            where
              = {
                def condition ( c : Condition ) : Sql.Condition[Sql.WhereObject]
                  = c match {
                      case And(l, r) => 
                        Sql.CompositeCondition( condition(l), condition(r), Sql.And )
                      case Or(l, r) => 
                        Sql.CompositeCondition( condition(l), condition(r), Sql.Or )
                      case Comparison(t, c, Equal, null) =>
                        Sql.IsNull( 
                          Sql.Column(c, Some(aliases(t)))
                        )
                      case Comparison(t, c, NotEqual, null) =>
                        Sql.IsNull(
                          Sql.Column(c, Some(aliases(t))), true
                        )
                      case Comparison(t, c, o, v) =>
                        Sql.Comparison(
                          Sql.Column(c, Some(aliases(t))),
                          Sql.Value(v),
                          sql(o)
                        )
                    }
                s.condition map condition
              }
          )
      }

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