package vorm.selectAbstraction

import vorm._
import extensions._
import structure._
import query._
import mapping.{Table => TableMapping, Seq => SeqMapping}
import vorm.{sql => Sql}

/**
 *  Usage:
 *    assuming rock and metal are `Style` entities
 *    Query:
 *      artists.filter(
 *          ( "styles" includes Set(rock, metal) ) ||
 *          ( "styles" is Set(pop) )
 *        )
 *    MappingSelect:
 *      ???
 */
case class MappingSelect
  ( mapping : TableMapping,
    // mappingsInSkeleton : Set[TableMapping],
    skeletonAliases : Map[TableMapping, String],

    sql : Sql.Select )
    // skeletonJoins : Map[TableMapping, Sql.Join] )
  {

    private def withSkeletonTo
      ( m : TableMapping )
      : MappingSelect
      = ???

    /**
     * This select with new select joined, but not binded  
     */
    private def withSelect
      ( s : MappingSelect )
      : ( MappingSelect, String )
      = copy(
            sql
              = sql.copy(
                    join
                      = sql.join :+
                        Sql.Join( s.sql, Some("t" + sql.join.length),
                              kind = Sql.JoinKind.Inner )
                  )
          ) → 
        "t" + sql.join.length


    def andWhere
      ( w : Query.Where )
      // = this intersection MappingSelect(w)
      = MappingSelect(w) match {
          case Some(s : MappingSelect) ⇒ 
            withSkeletonTo(s.mapping).withSelect(s) match { 
                case (s1, a) ⇒ 
                  s1.copy(
                      sql
                        = s1.sql.copy(
                              where
                                = ( s1.sql.where ++
                                    s.mapping.primaryKeyColumns.view
                                      .map(_.name)
                                      .map{ n ⇒ 
                                        Sql.Clause.Equals(
                                            Sql.Column(n, Some(a)),
                                            Sql.Column(n, Some(skeletonAliases(s.mapping)))
                                          )
                                      }
                                      .reduceOption( Sql.Clause.And )
                                    )
                                    .reduceOption( Sql.Clause.And )
                            )
                    )
              }
        }


    def orWhere
      ( w : Query.Where )
      = this union MappingSelect(w)

    /**
     * Mixes two selects with optimizations
     */
    private def intersection
      ( s : MappingSelect )
      : MappingSelect
      = ???

    private def union
      ( s : MappingSelect )
      : MappingSelect
      = ???

    private lazy val referredSubTables
      : Seq[TableMapping]
      = ???

  }

object MappingSelect {

  def apply
    ( w : Query.Where )
    = w match {
        case Query.Where.Equals( m : mapping.Value, v ) ⇒ 
          ???
        case Query.Where.Equals( m : SeqMapping, v : Seq[_] ) ⇒ 
          ???
          // ⇒ m.select.primaryKey
          //     .foldFrom(v) { (s, v) ⇒ 
          //       s.orWhere(
          //           Query.Where.Equals(m.child.child, v)
          //         )
          //     }
      }

}

