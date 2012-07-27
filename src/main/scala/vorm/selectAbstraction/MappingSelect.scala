package vorm.selectAbstraction

import vorm._
import extensions._
import structure._
import query._
import mapping.{Table => TableMapping, Seq => SeqMapping, Value => ValueMapping}
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
    skeletonAliases : Map[TableMapping, String] = Map(),
    joins : Vector[Sql.Join] = Vector(),
    where : Option[Sql.Clause] = None,
    groupBy : Vector[Sql.Column] = Vector(),
    having : Option[Sql.Clause] = None )
  {
    lazy val resultSetSql
      : Sql.Select
      = ???

    lazy val primaryKeySql
      : Sql.Select
      = ???

    private lazy val newAlias
      = "t" + joins.length

    private def withSkeletonTo
      ( m : Mapping )
      : MappingSelect
      = m match {
          case m : TableMapping ⇒ 
            if( m == mapping )
              this
            else if( skeletonAliases contains m )
              this
            else { 
              val s = withSkeletonTo( m.parentTableMapping.get )
              s.copy(
                  skeletonAliases 
                    = s.skeletonAliases + (m → s.newAlias),
                  joins 
                    = s.joins :+ 
                      Sql.Join( 
                          Sql.Table(m.tableName),
                          Some(s.newAlias),
                          m.primaryKeyColumns.view
                            .map(_.name)
                            .map{ n ⇒ 
                              Sql.Column(n, Some(s.newAlias)) → 
                              Sql.Column(n, Some(s.skeletonAliases(m.parentTableMapping.get)))
                            }
                            .toList
                        )
                )
            }
          case _ ⇒
            withSkeletonTo( m.parentTableMapping.get )
        }

    private def withSelect
      ( s : MappingSelect, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = copy(
            joins
              = joins :+
                Sql.Join( s.primaryKeySql, Some(newAlias),
                          kind = Sql.JoinKind.Inner ),
            where
              = ( where ++
                  s.mapping.primaryKeyColumns.view
                    .map{ _.name }
                    .map{ n ⇒ 
                        Sql.Clause.Equals(
                            Sql.Column(n, Some(newAlias)),
                            Sql.Column(n, Some(skeletonAliases(s.mapping)))
                          )
                      } 
                    .reduceOption{ Sql.Clause.And } )
                  .reduceOption{ o }
          )


    // def withWhere
    //   ( c : Option[Sql.Clause],
    //     o : (Sql.Clause, Sql.Clause) => Sql.Clause )
    //   = copy( where = ( where ++ c ) reduceOption o )


    def withWhere
      ( w : Query.Where.Filter, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      = MappingSelect(w)
          .map{ withSelect(_, o) }
          .getOrElse{ 
            copy( 
                where
                  = ( where ++ condition(w) ) 
                      .reduceOption(o)
              )
          }

    private def condition
      ( w : Query.Where.Filter )
      : Option[Sql.Clause]
      = w match {
          case Query.Where.Equals( m : ValueMapping, v ) ⇒ 
            Sql.Clause.Equals( 
                Sql.Column( m.columnName, 
                            Some( skeletonAliases(m.parentTableMapping.get) ) ),
                Sql.Value( v )
              )
              .some
        }

    def orWhere
      ( w : Query.Where.Filter )
      = withSkeletonTo(w.mapping)
          .withWhere(w, Sql.Clause.Or)


  }

object MappingSelect {
  def apply
    ( w : Query.Where )
    : Option[MappingSelect]
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

