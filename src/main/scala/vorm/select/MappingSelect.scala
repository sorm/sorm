package vorm.select

import vorm._
import persisted._
import structure._
import query._
import mapping._
import vorm.{sql => Sql}
import ddl._
import extensions._

case class MappingSelect
  ( mapping : TableMapping,
    resultMappings : Seq[(TableMapping, Column)] = Seq(),
    joinsAliases : Map[TableMapping, String] = Map(),
    joins : IndexedSeq[Sql.Join] = Vector(),
    where : Option[Sql.Clause] = None,
    groupBy : IndexedSeq[Sql.Column] = Vector(),
    having : Option[Sql.Clause] = None,
    orderBy : IndexedSeq[Sql.OrderByClause] = Vector(),
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
    //               .map{ n => 
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
    //     //         case Query.Order( m, r ) => 
    //     //           valueMappings(m)
    //     //             .view
    //     //             .map{ m => Sql.Column(m.columnName, 
    //     //                                  alias(m.ownerTable).some) }
    //     //             .map{ Sql.OrderByClause(_, r) }
    //     //             .toIndexedSeq
    //     //       }
    //     //   joins
    //     //     = 
    //     // )
    //   }
    def withOrders
      ( orders : Seq[Query.Order] )
      : MappingSelect
      = {
        def columns
          ( m : Mapping )
          : IndexedSeq[Sql.Column]
          = m match {
              case m : ValueMapping => 
                Vector() :+
                Sql.Column(m.columnName, alias(m.containerTableMapping.get).some)
              case m : TableMapping => 
                m.primaryKeyColumns
                  .view
                  .map{ c => Sql.Column(c.name, alias(m).some) }
                  .toIndexedSeq
              case m : HasChildren => 
                m.nestedValueMappings flatMap columns toIndexedSeq
            }
        copy(
          orderBy
            = orderBy ++
              orders.flatMap{ order => 
                columns(order.mapping)
                  .map{ Sql.OrderByClause(_, order.reverse) }
              }
        )
      }

    private def from
      = Sql.From(Sql.Table(mapping.tableName), Some(Sql.alias(0)))
    private def what
      = resultMappings.map{ case (m, c) => Sql.Column(c.name, alias(m).some) }

    private def alias
      ( m : TableMapping )
      = if( m == mapping ) Sql.alias(0)
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
            = m.nestedTableMappings.flatMap{ m => m +: subTables(m) }.toSeq

          mapping +: subTables( mapping )
        }

        allTables
          .foldLeft(this){_ withSkeletonTo _}
          .copy(
            resultMappings = allTables.flatMap{ m => m.columns.map{m → _} }
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


    private lazy val newAlias = Sql.alias( joins.length + 1 )

    private def withSkeletonTo
      ( m : Mapping )
      : MappingSelect
      = m match {
          // case m if m == mapping => 
          //   copy( joinsAliases = joinsAliases + (mapping → Sql.alias(0)) )
          // case m : TableMapping if joinsAliases contains m => 
          //   this
          // case m : CollectionMapping => 
          //   val s = withSkeletonTo( m.containerTableMapping.get )
          //   s.copy(
          //     joinsAliases
          //       = s.joinsAliases + (m → s.newAlias),
          //     joins
          //       = s.joins :+
          //         Sql.Join(
          //           Sql.Table(m.tableName),
          //           Some(s.newAlias),

          //         )
          //   )

          case m : TableMapping => 
            def bindingsToContainer
              ( m : TableMapping )
              = m match {
                  case m : CollectionTableMapping => 
                    m.containerTableMappingForeignKey.get.bindings.view
                  case m => 
                    m.containerTableMapping.get.foreignKeys(m)
                      .bindings.view.map{_.swap}
                }

            if( m == mapping )
              copy( joinsAliases = joinsAliases + (mapping → Sql.alias(0)) )
            else if( joinsAliases contains m )
              this
            else { 
              val s = withSkeletonTo( m.containerTableMapping.get )
              s.copy(
                joinsAliases
                  = s.joinsAliases + (m → s.newAlias),
                joins
                  = s.joins :+
                    Sql.Join(
                      Sql.Table(m.tableName),
                      Some(s.newAlias),
                      bindingsToContainer(m)
                        .map{ b =>
                          Sql.Column(b._1, Some(s.newAlias)) →
                          Sql.Column(b._2, Some(s.joinsAliases(m.containerTableMapping.get)))
                        }
                        .toList
                    )
              )
            }
          case _ =>
            withSkeletonTo( m.containerTableMapping.get )
        }

    private def withSelect
      ( s : MappingSelect, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause
        )
      : MappingSelect
      = copy(
            joins
              = joins :+
                Sql.Join( s.sql, Some(newAlias),
                          kind = Sql.JoinKind.Inner ),
            where
              = ( where ++
                  s.resultMappings.view
                    .map{ _._2 }
                    .map{ _.name }
                    .map{ n =>
                        Sql.Clause.Equals(
                            Sql.Column(n, Some(newAlias)),
                            //  TODO: optimize to bind to parent
                            Sql.Column(n, Some(alias(s.mapping)))
                          )
                      } 
                    .reduceOption{ Sql.Clause.And } )
                  .reduceOption{ o }
          )

    private def withSelect1
      ( s : MappingSelect,
        on : Traversable[(String, String)],
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = copy(
            joins
              = joins :+
                Sql.Join( s.sql, Some(newAlias),
                          kind = Sql.JoinKind.Inner ),
            where
              = ( where ++
                  on.map{ case (l, r) =>
                      Sql.Clause.Equals(
                        Sql.Column(l, Some(newAlias)),
                        //  TODO: optimize to bind to parent
                        Sql.Column(r, Some(alias(s.mapping)))
                      )
                    }
                    .reduceOption{ Sql.Clause.And } )
                  .reduceOption{ o }
          )


    def havingRowsCount
      ( r : Int,
        o : (Sql.ConditionObject, Sql.ConditionObject) => Sql.Clause.Condition 
          = Sql.Clause.Equals )
      : MappingSelect
      = copy(
          having
            = ( having ++ 
                Some(
                  o(
                    Sql.Count(
                      mapping.primaryKeyColumns
                        .map{ c => Sql.Column(c.name, Some(Sql.alias(0))) },
                      true
                    ),
                    Sql.Value(r)
                  )
                )
              ) reduceOption Sql.Clause.And,
          groupBy
            = groupBy ++
              mapping.asInstanceOf[CollectionTableMapping].containerTableColumns
                .map{ c => Sql.Column(c.name, Some(Sql.alias(0))) }

        )

    def andFilter
      ( w : Query.Filter )
      = withSkeletonTo( w.mapping )
          .withFilter( w, Sql.Clause.And )

    def orFilter
      ( w : Query.Filter )
      = withSkeletonTo( w.mapping )
          .withFilter( w, Sql.Clause.Or )

    def withFilter
      ( w : Query.Filter, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause = Sql.Clause.And )
      : MappingSelect
      = withSkeletonTo(w.mapping).withFilter1(w, o)

    private def withFilter1
      ( f : Query.Filter, 
        o : (Sql.Clause, Sql.Clause) => Sql.Clause )
      : MappingSelect
      = {
        import Query._
        import Operator._

        f match {
          case Filter( HasSize, m : CollectionTableMapping, v : Int ) => 
            withSelect( MappingSelect(m).primaryKey.havingRowsCount(v), o )
          case Filter( Equals, m : SeqMapping, v : Seq[_] ) =>
            lazy val countSelect
              = MappingSelect(
                  m,
                  m.containerTableColumns.map{ m -> _ },
                  having
                    = ( having ++
                        Some(
                          Sql.Clause.Equals(
                            Sql.Count(
                              Sql.Column("i", Some(Sql.alias(0))) :: Nil,
                              true ),
                            Sql.Value(v.length)
                          )
                        )
                      ) reduceOption Sql.Clause.And,
                  groupBy
                    = m.containerTableColumns.view
                        .map{_.name}
                        .map{Sql.Column(_, Some(Sql.alias(0)))}
                        .toIndexedSeq
                )
            withSelect(
              v.zipWithIndex.view.foldLeft(countSelect){ case (s, (v, i)) =>
                  s.withWhere(
                    And(
                      Filter(Equals, m.index, i),
                      Filter(Equals, m.item, v)
                    ),
                    Sql.Clause.Or
                  )
                }
                .withSelect(countSelect, Sql.Clause.And)
            , o
            )
          case Filter( NotEquals, m : SeqMapping, v : Seq[_] ) => 
            v .view
              .zipWithIndex
              .foldLeft( MappingSelect(m).primaryKey ){ case (s, (v, i)) =>
                s .withClause(
                    Sql.Clause.NotEquals( 
                      Sql.Column("i", s.alias(m).some),
                      Sql.Value(i) ),
                    Sql.Clause.Or ) 
                  .withFilter(
                    Filter( NotEquals, m.item, v ),
                    Sql.Clause.Or )
              }
              .withSelect(
                MappingSelect(m).primaryKey.havingRowsCount(
                  v.length, Sql.Clause.NotEquals ),
                Sql.Clause.Or )
              .as{ withSelect(_, o) }
          case Filter( Contains, m : SeqMapping, v ) => 
            withFilter1( Filter( Includes, m, Seq(v) ), o )
          case Filter( Includes, m : SeqMapping, v : Seq[_] ) =>
            withSelect(
                MappingSelect(m).primaryKey
                  .foldFrom(v){ (s, v) => 
                      s.withFilter( Filter( Equals, m.item, v ),
                                    Sql.Clause.Or )
                    }
                  .havingRowsCount(v.length),
                o
              )
          case Filter( Contains, m : SetMapping, v ) =>
            withFilter1( Filter( Includes, m, Set(v) ), o )
          case Filter( Equals, m : SetMapping, v : Set[_] ) => 
            withWhere( And( Filter( Includes, m, v ),
                            Filter( HasSize, m, v ) ),
                       o )
          case Filter( op, m : OptionMapping, Some(v) ) =>
            withFilter1( f.copy(mapping = m.item, value = v), o )
          case Filter( op, m : OptionMapping, None ) =>
            withFilter1( f.copy(mapping = m.item, value = null), o )
          case Filter( op, m : ValueMapping, v ) =>
            withCondition( m, v, conditionOperator(op), o )
          case Filter( op, m : EntityMapping, v : Persisted ) =>
            withSkeletonTo(m)
              .withCondition( m.id, v.id, conditionOperator(op), o )
          case Filter( _, m : EntityMapping, _ ) =>
            throw new Exception("Only persisted entities can be used in filters")
        }
    }

    private def conditionOperator
      ( f : Query.Operator )
      : (Sql.ConditionObject, Sql.ConditionObject) => Sql.Clause.Condition
      = f match {
          case Query.Operator.Equals           => Sql.Clause.Equals
          case Query.Operator.NotEquals        => Sql.Clause.NotEquals
          case Query.Operator.Larger           => Sql.Clause.Larger
          case Query.Operator.LargerIncluding  => Sql.Clause.LargerIncluding
          case Query.Operator.Smaller          => Sql.Clause.Smaller
          case Query.Operator.SmallerIncluding => Sql.Clause.SmallerIncluding
          case Query.Operator.Like             => Sql.Clause.Like
          case Query.Operator.Regex            => Sql.Clause.Regex
          case Query.Operator.In               => Sql.Clause.In
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
                    Some( alias(m.containerTableMapping.get) )
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
          case Query.Or(l, r) =>
            copy( where = None )
              .withWhere(l)
              .withWhere(r, Sql.Clause.Or)
              .foldFrom(where){ _ withClause (_, o) }
              // .foldFrom(where){ (s, c) => s withClause (c, o) }
          case Query.And(l, r) =>
            copy( where = None )
              .withWhere(l)
              .withWhere(r, Sql.Clause.And)
              .foldFrom(where){ _ withClause (_, o) }
          case f: Query.Filter => 
            withFilter(f, o)
        }
  }
