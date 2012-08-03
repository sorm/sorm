package vorm.select

import vorm._
import structure._
import query._
import mapping._
import vorm.{sql => Sql}
import ddl._
import extensions._

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
    resultMappings : Seq[(TableMapping, Column)] = Seq(),
    joinsAliases : Map[TableMapping, String] = Map(),
    joins : Vector[Sql.Join] = Vector(),
    where : Option[Sql.Clause] = None,
    groupBy : Vector[Sql.Column] = Vector(),
    having : Option[Sql.Clause] = None,
    orderBy : Vector[Sql.OrderByClause] = Vector(),
    limit : Option[Int] = None,
    offset : Option[Int] = None )
  {
    // def withQuery
    //   ( q : Query )
    //   = {

    //     val s1 = q.order.foldLeft(this){_ withOrder _}
    //     s1.copy(
    //       joins
    //         = s1.joins :+
    //           Sql.Join( s.sql, Some(newAlias),
    //                     kind = Sql.JoinKind.Inner ),
    //       where
    //         = ( where ++
    //             s.mapping.primaryKeyColumns.view
    //               .map{ _.name }
    //               .map{ n ⇒ 
    //                   Sql.Clause.Equals(
    //                       Sql.Column(n, Some(newAlias)),
    //                       //  TODO: optimize to bind to parent
    //                       Sql.Column(n, Some(joinsAliases(s.mapping)))
    //                     )
    //                 } 
    //               .reduceOption{ Sql.Clause.And } )
    //             .reduceOption{ o }
    //     )

    //     // copy(
    //     //   orderBy 
    //     //     = q.order.flatMap {
    //     //         case Query.Order( m, r ) ⇒ 
    //     //           valueMappings(m)
    //     //             .view
    //     //             .map{ m ⇒ Sql.Column(m.columnName, 
    //     //                                  alias(m.ownerTable).some) }
    //     //             .map{ Sql.OrderByClause(_, r) }
    //     //             .toIndexedSeq
    //     //       }
    //     //   joins
    //     //     = 
    //     // )
    //   }
    def withOrder
      ( order : Query.Order )
      : MappingSelect
      = {
        def valueMappings
          ( m : Mapping )
          : Iterable[ValueMapping]
          = m match {
              case m : HasChildren ⇒ 
                // m.leaves flatMap valueMappings
                m.nestedValueMappings
              case m : ValueMapping ⇒ 
                m :: Nil
            }

        valueMappings( order.mapping )
          .foldLeft(this){
            case (s, m) ⇒ 
              val s1 = s.withSkeletonTo(m)
              s1.copy(
                orderBy
                  = s1.orderBy :+
                    Sql.OrderByClause(
                      Sql.Column(m.columnName, alias(m.ownerTable.get).some),
                      order.reverse
                    )
              )
          }

      }

    private def from
      = Sql.From(Sql.Table(mapping.tableName), Some("a"))
    private def what
      = resultMappings.map{ case (m, c) ⇒ Sql.Column(c.name, alias(m).some) }

    private def alias
      ( m : TableMapping )
      = if( m == mapping ) "a"
        else joinsAliases(m)

    def sql
      : Sql.Select
      = Sql.Select(what, from, joins, where, groupBy, having, orderBy, limit, offset)

    def resultSet
      : MappingSelect
      = {

        val allTables = {
          def subTables
            ( m : TableMapping )
            : Seq[TableMapping]
            = m.subTableMappings.flatMap{ m ⇒ m +: subTables(m) }.toSeq

          mapping +: subTables( mapping )
        }

        allTables
          .foldLeft(this){_ withSkeletonTo _}
          .copy(
            resultMappings = allTables.flatMap{ m ⇒ m.valueColumns.map{m → _} }
          )
      }

    def primaryKey
      : MappingSelect
      = copy(
          resultMappings
            = mapping.primaryKeyColumns
                .view
                .map{mapping → _}
                .toSeq
        )


    private lazy val newAlias
      = ( joins.length + 98 ).toChar.toString

    private def withSkeletonTo
      ( m : Mapping )
      : MappingSelect
      = m match {
          case m : TableMapping ⇒ 
            if( m == mapping )
              copy(joinsAliases = joinsAliases + (mapping -> "a"))
            else if( joinsAliases contains m )
              this
            else { 
              val s = withSkeletonTo( m.ownerTable.get )
              s.copy(
                  joinsAliases 
                    = s.joinsAliases + (m → s.newAlias),
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
                                     Sql.Column(b._2, Some(s.joinsAliases(m.ownerTable.get)))}
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
                            Sql.Column(n, Some(joinsAliases(s.mapping)))
                          )
                      } 
                    .reduceOption{ Sql.Clause.And } )
                  .reduceOption{ o }
          )


    def havingRowsCount
      ( r : Int )
      : MappingSelect
      = copy(
          having
            = ( having ++ 
                Some(
                  Sql.Clause.Equals(
                    Sql.Count(
                      mapping.primaryKeyColumns
                        .map{ c ⇒ Sql.Column(c.name, Some("a")) },
                      true
                    ),
                    Sql.Value(r)
                  )
                )
              ) reduceOption Sql.Clause.And,
          groupBy
            = groupBy ++
              mapping.primaryKeyColumns
                .map{ c ⇒ Sql.Column(c.name, Some("a")) }

        )

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
                            Sql.Column("i", s.joinsAliases(m).some),
                            Sql.Value(i)
                          )
                      )
                      .withFilter(
                          Query.Where.Equals(m.item, v),
                          Sql.Clause.Or
                        )
                  }
                  .havingRowsCount(v.length)
                  .withSkeletonTo(m)
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
          case f : Query.Where.Equals     ⇒ Sql.Clause.Equals
          case f : Query.Where.NotEquals  ⇒ Sql.Clause.NotEquals
          case f : Query.Where.In         ⇒ Sql.Clause.In
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
                    Some( joinsAliases(m.ownerTable.get) )
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
  }

object MappingSelect {

//  /**
//   * Has all the skeleton mappings applied.
//   */
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

//  def apply
//    ( q : Query )
//    : MappingSelect
//    = ???

  // def sqlAndResultSetBindings
  //   ( q : Query )

}

