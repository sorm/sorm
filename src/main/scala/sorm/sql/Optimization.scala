package sorm.sql

import sext._, embrace._
import sorm.sql.Sql._

object Optimization {
  def optimized ( s : Statement ) : Statement
    = s match {
        case Union(l, r) => Union(optimized(l), optimized(r))
        case s : Select => 
          s.copy(
            join = s.join map {
              case j @ Join(what : Statement, _, _, _) =>
                j.copy(what $ optimized)
              case j => j
            }
          ) $ groupByToDistinct
      }
  private def groupByToDistinct ( select : Select ) : Select
    = if( select.groupBy.toSet == select.what.toSet && select.having.isEmpty )
        select.copy(groupBy = Nil, distinct = true)
      else
        select

  /**
   * Not finished
   */
  private def dropOrphans ( select : Select ) : Select
    = {
      val refs
        : Set[String]
        = {
          val whatRefs
            = select.what.view collect { case Column(_, Some(r)) ⇒ r }
          val fromRef
            = select.from.as
          val whereRefs
            = ???
          val groupByRefs
            = select.groupBy collect { case Column(_, Some(r)) ⇒ r }
          val havingRefs
            = ???

          Set() ++ whatRefs ++ fromRef ++ whereRefs ++ groupByRefs ++ havingRefs
        }

      def f
        ( s : Select )
        : Select
        = {
          val joinRefs
            = s.join.view flatMap {
                _.on collect { case (_, Column(_, Some(r))) ⇒ r }
              }

          val allRefs
            = refs ++ joinRefs

          val filtered
            = s.join filter {
                _.as map { allRefs contains _ } getOrElse false
              }

          if( filtered == s.join )
            s
          else
            f( s copy ( join = filtered ) )
        }

      def withSubSelectsOptimized
        ( s : Select )
        = s.copy(
              join
                = s.join map { j ⇒
                    j.what match {
                      case s : Select ⇒ j.copy(s $ dropOrphans)
                      case _ ⇒ j
                    }
                  }
            )

      withSubSelectsOptimized( f(select) )
    }
}
