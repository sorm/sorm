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
        case Select(expressions, condition, havingCount) =>

          lazy val tables : Set[Table]
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
              expressions.view.collect{ case Column(n, t) => t } ++
              condition.toSeq.flatMap{conditionTables} ++
              havingCount.map{_.table} 
            }

          lazy val tableParents : Map[Table, Seq[Table]]
            = tables
                .zipBy{
                  _.unfold{
                    _.parent.map{_.table}.map{t => t -> t }
                  }
                }
                .toMap

          lazy val leafTables : Set[Table]
            = tables
                .filter{ t => 
                  tables.exists{ ! tableParents(_).contains(t) }
                }

          lazy val allTables : Seq[Table]
            = tables
                .foldRight( Stream[Table]() ){ (t, acc) =>
                  tableParents(t) ++: acc
                }
                .reverse
                .distinct

          lazy val aliases : Map[Table, String]
            = allTables.view.zipWithIndex
                .map{ case(t, i) => t -> alias(i) }
                .toMap

          Sql.Select(
            what
              = expressions.view
                  .collect{ 
                    case Column(n, t) => 
                      Sql.Column(n, Some(aliases(t)))
                  },
            from
              = Sql.From(Sql.Table(tables.head.name), 
                         Some(aliases(tables.head))),
            join
              = allTables.view.tail
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
                  }
          )
      }


}