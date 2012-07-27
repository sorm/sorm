package vorm.selectAbstraction

import vorm._
import extensions._
import structure._
import query._
import mapping.{Table => TableMapping, Seq => SeqMapping}

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
  ( mapping : TableMapping )
  {

    def andWhere
      ( w : Query.Where )
      = this intersection MappingSelect(w)

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

