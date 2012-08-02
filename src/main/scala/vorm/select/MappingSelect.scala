package vorm.select

import vorm._
import extensions._
import structure._
import query._
import mapping._
import vorm.{sql => Sql}

/**
 *  Usage:
 *    assuming `rock` and `metal` are `Style` entities
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
    what : Vector[Sql.SelectObject] = Vector(),
    joins : Vector[Sql.Join] = Vector(),
    where : Option[Sql.Clause] = None,
    groupBy : Vector[Sql.Column] = Vector(),
    having : Option[Sql.Clause] = None )
  {
    lazy val resultSet
      : MappingSelect
      = ???

    lazy val primaryKey
      : MappingSelect
      = ???

    private def from
      = Sql.From(Sql.Table(mapping.tableName), Some("a"))

    lazy val sql
      : Sql.Select
      = Sql.Select(what, from, joins, where, groupBy, having)

    private lazy val newAlias
      = ( joins.length + 98 ).toChar.toString

    def withSkeletonTo
      ( m : Mapping )
      : MappingSelect
      = m match {
          case m : TableMapping ⇒ 
            if( m == mapping )
              copy(skeletonAliases = skeletonAliases + (mapping -> "a"))
            else if( skeletonAliases contains m )
              this
            else { 
              val s = withSkeletonTo( m.ownerTable.get )
              s.copy(
                  skeletonAliases 
                    = s.skeletonAliases + (m → s.newAlias),
                  joins 
                    = s.joins :+ 
                      Sql.Join( 
                          Sql.Table(m.tableName),
                          Some(s.newAlias),
                          m.foreignKeyForOwnerTable
                            .map{_.bindings.map{_.swap}}
                            .getOrElse(m.ownerTableForeignKey.get.bindings)
                            .view
                            .map{b ⇒ Sql.Column(b._1, Some(s.newAlias)) → 
                                     Sql.Column(b._2, Some(s.skeletonAliases(m.ownerTable.get)))}
                            .toList
                        )
                )
            }
          case _ ⇒
            withSkeletonTo( m.ownerTable.get )
        }

    private def withSelect
      ( s : MappingSelect, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = copy(
            joins
              = joins :+
                Sql.Join( s.sql, Some(newAlias),
                          kind = Sql.JoinKind.Inner ),
            where
              = ( where ++
                  s.mapping.primaryKeyColumns.view
                    .map{ _.name }
                    .map{ n ⇒ 
                        Sql.Clause.Equals(
                            Sql.Column(n, Some(newAlias)),
                            //  TODO: optimize to bind to parent
                            Sql.Column(n, Some(skeletonAliases(s.mapping)))
                          )
                      } 
                    .reduceOption{ Sql.Clause.And } )
                  .reduceOption{ o }
          )


    def havingRowsCount
      ( r : Int )
      : MappingSelect
      = ???

    def andFilter
      ( w : Query.Where.Filter )
      = withSkeletonTo( w.mapping )
          .withFilter( w, Sql.Clause.And )

    def orFilter
      ( w : Query.Where.Filter )
      = withSkeletonTo( w.mapping )
          .withFilter( w, Sql.Clause.Or )

    def withFilter
      ( w : Query.Where.Filter, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause = Sql.Clause.And )
      : MappingSelect
      = withSkeletonTo(w.mapping).withFilter1(w, o)

    private def withFilter1
      ( w : Query.Where.Filter, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = w match {
          case Query.Where.Contains( m : SeqMapping, v ) ⇒ 
            withFilter1( Query.Where.Includes( m, Seq(v) ), o )
          case Query.Where.Includes( m : SeqMapping, v : Seq[_] ) ⇒ 
            withSelect(
                MappingSelect(m).primaryKey
                  .foldFrom(v){ (s, v) ⇒ 
                      s.withFilter( Query.Where.Equals(m.item, v),
                                    Sql.Clause.Or )
                    }
                  .havingRowsCount(v.length),
                o
              )
          case Query.Where.Equals( m : SeqMapping, v : Seq[_] ) ⇒ 
            withSelect(
                v.view.zipWithIndex
                  .foldLeft( MappingSelect(m).primaryKey ){ case (s, (v, i)) ⇒
                    s.withClause( 
                        Sql.Clause.Equals(
                            Sql.Column("i", s.skeletonAliases(m).some),
                            Sql.Value(i)
                          )
                      )
                      .withFilter(
                          Query.Where.Equals(m.item, v),
                          Sql.Clause.Or
                        )
                  }
                  .havingRowsCount(v.length)
                  .withSelect( 
                      MappingSelect(m).primaryKey.havingRowsCount(v.length), 
                      Sql.Clause.And 
                    ),
                o
              )
          case Query.Where.HasSize( m : SeqMapping, v : Int ) ⇒ 
            withSelect( MappingSelect(m).primaryKey.havingRowsCount(v), o )
          case Query.Where.Equals( m : SetMapping, v : Seq[_] ) ⇒ 
            withWhere(
                Query.Where.And(
                    Query.Where.Includes(m, v),
                    Query.Where.HasSize(m, v)
                  ),
                o
              )
          case f : Query.Where.Filter ⇒ 
            f.mapping match {
              case m : ValueMapping ⇒ 
                withCondition( m, f.value, conditionOperator(f), o )
            }
        }

    private def conditionOperator
      ( f : Query.Where.Filter )
      : (Sql.ConditionObject, Sql.ConditionObject) => Sql.Clause.Condition
      = f match {
          case f : Query.Where.Equals ⇒ Sql.Clause.Equals
          case f : Query.Where.NotEquals ⇒ Sql.Clause.NotEquals
          case f : Query.Where.In => Sql.Clause.In
        }

    private def withCondition
      ( m : ValueMapping,
        v : Any,
        cf : (Sql.ConditionObject, Sql.ConditionObject) => Sql.Clause.Condition,
        of : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = withClause(
            cf(
                Sql.Column( 
                    m.columnName,
                    Some( skeletonAliases(m.ownerTable.get) )
                  ),
                Sql.Value(v)
              ),
            of
          )

    private def withClause
      ( c : Sql.Clause,
        o : (Sql.Clause, Sql.Clause) => Sql.Clause = Sql.Clause.And )
      = copy(
            where
              = (where ++ Some(c)).reduceOption{ o }
          )

    def withWhere
      ( w : Query.Where, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause = Sql.Clause.And )
      : MappingSelect
      = w match {
          case Query.Where.Or(l, r) ⇒ 
            copy( where = None )
              .withWhere(l)
              .withWhere(r, Sql.Clause.Or)
              .foldFrom(where){ _ withClause (_, o) }
              // .foldFrom(where){ (s, c) ⇒ s withClause (c, o) }
          case Query.Where.And(l, r) ⇒ 
            copy( where = None )
              .withWhere(l)
              .withWhere(r, Sql.Clause.And)
              .foldFrom(where){ _ withClause (_, o) }
          case f: Query.Where.Filter ⇒ 
            withFilter(f, o)
        }

    def withQuery
      ( q : Query )
      = q match {
          case Query(kind, this.mapping, where, order, limit) =>
            where.map(withWhere(_))
            ???
        }

  }

object MappingSelect {

  /**
   * Has all the skeleton mappings applied. 
   */
//  def resultSetReady
//    ( m : TableMapping )
//    : MappingSelect
//    = {
//      def leaves
//        ( m : Mapping )
//        : Seq[Mapping]
//        = m match {
//            case m : mapping.HasChildren ⇒ m.children.flatMap(leaves)
//            case m : mapping.HasChild ⇒ leaves(m.child)
//            case _ ⇒ Vector(m)
//          }
//
//      leaves(m).foldLeft(MappingSelect(m)){ _ withSkeletonTo _ }
//    }

  def apply
    ( q : Query )
    = ???
}

