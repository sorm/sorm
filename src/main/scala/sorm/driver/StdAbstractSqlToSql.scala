package sorm.driver

import sext._, embrace._

import sorm._
import sql._, Compositing._
import abstractSql._, AbstractSql._

trait StdAbstractSqlToSql {
  def sql
    ( statement : Statement )
    : Sql.Statement
    = statement match {
        case Union(l, r) =>
          Sql.Union( sql(l), sql(r) )
        case Intersection(l, r) =>
          sql(l) narrow sql(r)
        case s : Select =>

          val tables = SqlComposition.allTables(s)

          val aliases : Map[Table, String]
            = tables.view.zipWithIndex
                .map{ case(t, i) => t -> alias(i) }
                .toMap

          Sql.Select(
            what
              = s.expressions.toStream
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
                          SqlComposition.sql(o)
                        )
                      case EverTrue =>
                        Sql.Comparison( Sql.Value(true), Sql.Value(true), Sql.Equal )
                      case EverFalse =>
                        Sql.Comparison( Sql.Value(true), Sql.Value(false), Sql.Equal )
                    }
                s.condition map condition
              },
            groupBy
              = s.groupBy.toStream
                  .map{ case Column(n, t) =>
                    Sql.Column(n, Some(aliases(t)))
                  },
            having
              = s.having
                  .map{ case HavingCount(t, n, o, c) =>
                    Sql.Comparison(
                      Sql.Count(
                        Sql.Column(n, Some(aliases(t))) :: Nil,
                        true
                      ),
                      Sql.Value(c),
                      SqlComposition.sql(o)
                    )
                  }
                  .asInstanceOf[Seq[Sql.Condition[Sql.HavingObject]]]
                  .reduceOption(Sql.CompositeCondition(_, _, Sql.And)),
            orderBy
              = s.order
                  .map{ case Order(t, n, r) =>
                    Sql.OrderBy( Sql.Column(n, Some(aliases(t))), r )
                  },
            limit
              = s.limit,
            offset
              = s.offset.satisfying{ _ != 0 }
          )
      }
}
