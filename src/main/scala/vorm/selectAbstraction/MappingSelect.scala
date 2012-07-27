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

    lazy val sql
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
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
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
                      s.withFilter( Query.Where.Equals(m.child.child, v),
                                    Sql.Clause.Or )
                    }
                  .havingRowsCount(v.length),
                o
              )
          case Query.Where.Equals( m : SeqMapping, v : Seq[_] ) ⇒ 
            MappingSelect(m).primaryKey
              .foldFrom(v){ (s, v) ⇒ 
                  s.withFilter( Query.Where.Equals(m.child.child, v),
                                Sql.Clause.Or )
                }
              .havingRowsCount(v.length)
              .withSelect( 
                  MappingSelect(m).primaryKey.havingRowsCount(v.length), 
                  Sql.Clause.And 
                )
          case Query.Where.HasSize( m : SeqMapping, v : Int ) ⇒ 
            withSelect( MappingSelect(m).primaryKey.havingRowsCount(v), o )
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
        }

    private def withCondition
      ( m : ValueMapping,
        v : Any,
        cf : (Sql.ConditionObject, Sql.ConditionObject) => Sql.Clause.Condition,
        of : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = withCondition(
            cf(
                Sql.Column( 
                    m.columnName,
                    Some( skeletonAliases(m.parentTableMapping.get) )
                  ),
                Sql.Value(v)
              ),
            of
          )

    private def withCondition
      ( c : Sql.Clause.Condition,
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      = copy(
            where
              = (where ++ Some(c)).reduceOption{ o }
          )

  }

object MappingSelect {

  /**
   * Has all the skeleton mappings applied. 
   */
  def resultSetReady
    ( m : TableMapping )
    : MappingSelect
    = {
      def leaves
        ( m : Mapping )
        : Seq[Mapping]
        = m match {
            case m : mapping.HasChildren ⇒ m.children.flatMap(leaves)
            case m : mapping.HasChild ⇒ leaves(m.child)
            case _ ⇒ Vector(m)
          }

      leaves(m).foldLeft(MappingSelect(m)){ _ withSkeletonTo _ }
    }

  def apply
    ( q : Query )
    = ???
}

