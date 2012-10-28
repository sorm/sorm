package sorm.abstractSql

import sext._, embrace._
import sorm._
import mappings._
import core._
import AbstractSql._
import Compositing._

object Combinators {

  implicit class StatementOps ( self : Statement ) {
    def | (other : Statement) 
      = union(self, other)
    def || (other : Traversable[Statement])
      = ( self /: other ) ( union )
    def |! (other : Statement)
      = Union(self, other)
    def ||! (other : Traversable[Statement])
      = ( self /: other ) ( Union )
    def & (other : Statement)
      = intersection(self, other)
    def && (other : Traversable[Statement])
      = ( self /: other ) ( intersection )
    def &! (other : Statement)
      = Intersection(self, other)
    def &&! (other : Traversable[Statement])
      = ( self /: other ) ( Intersection )
  }

  def restrictingCount
    ( s : Select, m : TableMapping, v : Int, o : Operator = Equal )
    : Select
    = s.copy(
        having
          = HavingCount(
              m.abstractSqlTable,
              m.primaryKeyColumns.last.name,
              o,
              v
            ) +:
            s.having
      )
  def havingCount ( m : TableMapping, v : Int, o : Operator = Equal ) : Select
    = restrictingCount( empty(m), m, v, o )

  def empty ( m : Mapping ) : Select
    = m.root.primaryKeySelect

  def havingNotEmptyContainer ( m : Mapping ) : Option[Select]
    = m.containerTableMapping.map{ havingCount(_, 0, NotEqual) }

  def including ( m : TableMapping, v : Iterable[_] ) : Option[Statement]
    = {
      val item
        = m match {
            case m : SeqMapping => m.item
            case m : SetMapping => m.item
            case m : MapMapping => m.key
            case _ => throw new SormException("including is unsupported by mapping `" + m + "`")
          }
      v.view
        .map{ equaling(item, _) }
        .reduceOption{ union }
        .asInstanceOf[Option[Select]]
        .map{ restrictingCount(_, m, v.size) }
    }

  def havingCondition ( m : Mapping, c : Condition )
    = empty(m).copy( condition = Some(c) )

  def comparing ( m : Mapping, o : Operator, v : Any )
    = empty(m).copy(
        condition
          = Some(
              Comparison(
                m.containerTableMapping.get.abstractSqlTable,
                m.memberName, o, v
              )
            )
      )

  def equaling ( m : Mapping, v : Any ) : Statement
    = (m, v) match {
        case (m : ColumnMapping, v) =>
          def theValue
            = (m, v) match {
                case (m : EnumMapping, v : Enumeration#Value) => m.dbValues(v) 
                case _ => v
              }
          empty(m).copy(
            condition
              = Some(
                  Comparison(
                    m.containerTableMapping.get.abstractSqlTable,
                    m.memberName,
                    Equal,
                    theValue
                  )
                )
          )
        case (m : EntityMapping, v : Persisted) =>
          equaling(m.id, v.id)
        case (m : RangeMapping, v : Range ) =>
          equaling(m.start, v.start) & equaling(m.end, v.end)
        case (m : TupleMapping, v : Product ) =>
          v.productIterator.zipWithIndex
            .map{ case (v, i) => equaling(m.items(i), v) }
            .reduce{_ & _}
        case (m : OptionToNullableMapping, v : Option[_] ) =>
          equaling(m.item, v.orNull)
        case (m : OptionToTableMapping, None ) =>
          havingCount( m, 0 ) &&!
          havingNotEmptyContainer(m)
        case (m : OptionToTableMapping, Some(v) ) =>
          equaling(m.item, v) &&!
          havingNotEmptyContainer(m)
        case (m : SeqMapping, v : Seq[_]) =>
          def crossingWithIndex
            = v.view.zipWithIndex
                .map{case (v, i) => equaling(m.index, i) & equaling(m.item, v)}
                .reduceOption{_ | _}
                .asInstanceOf[Option[Select]]

          havingCount( m, v.size ) &&!
          havingNotEmptyContainer(m) &&!
          crossingWithIndex.map{ restrictingCount(_, m, v.size) }
        case (m : SetMapping, v : Set[_]) =>
          havingCount( m, v.size ) &&!
          havingNotEmptyContainer(m) &&!
          including(m, v) 
        case (m : MapMapping, v : Map[_, _]) =>
          def crossingWithKey
            = v.view
                .map{case (k, v) => equaling(m.key, k) & equaling(m.value, v)}
                .reduceOption{_ | _}
                .asInstanceOf[Option[Select]]

          havingCount( m, v.size ) &&!
          havingNotEmptyContainer(m) &&!
          crossingWithKey.map{ restrictingCount(_, m, v.size) }
      }

  def notEqualing ( m : Mapping, v : Any ) : Statement
    = (m, v) match {
        case (m : ColumnMapping, v) =>
          def theValue
            = (m, v) match {
                case (m : EnumMapping, v : Enumeration#Value) => m.dbValues(v) 
                case _ => v
              }
          empty(m).copy(
            condition
              = Some(
                  Comparison(
                    m.containerTableMapping.get.abstractSqlTable,
                    m.memberName,
                    NotEqual,
                    theValue
                  )
                ) ++
                ( if( m.nullable && v != null )
                    Some(
                      Comparison(
                        m.containerTableMapping.get.abstractSqlTable,
                        m.memberName,
                        Equal,
                        null
                      )
                    )
                  else None
                ) reduceOption Or
          )
        case (m : EntityMapping, v : Persisted) =>
          notEqualing(m.id, v.id)
        case (m : RangeMapping, v : Range ) =>
          notEqualing(m.start, v.start) | notEqualing(m.end, v.end)
        case (m : TupleMapping, v : Product ) =>
          v.productIterator.zipWithIndex
            .map{ case (v, i) => notEqualing(m.items(i), v) }
            .reduce{_ | _}
        case (m : OptionToNullableMapping, v : Option[_] ) =>
          notEqualing(m.item, v.orNull)
        case (m : OptionToTableMapping, None ) =>
          havingCount(m, 1)
        case (m : OptionToTableMapping, Some(v) ) =>
          notEqualing(m.item, v) |! havingCount(m, 0)
        case (m : SeqMapping, v : Seq[_]) =>
          def disjoint
            = v.view.zipWithIndex
                .map{case (v, i) => 
                  equaling(m.index, i) & notEqualing(m.item, v)
                }
                .reduceOption{_ | _}
                .asInstanceOf[Option[Select]]

          def notMatchingSize
            = havingCount(m, v.size, NotEqual)

          notMatchingSize ||! disjoint
        case (m : SetMapping, v : Set[_]) =>
          def disjoint
            = v.view
                .map{ notEqualing(m.item, _) }
                .reduceOption{_ | _}
                .asInstanceOf[Option[Select]]

          def notMatchingSize
            = havingCount(m, v.size, NotEqual)

          notMatchingSize ||! disjoint
        case (m : MapMapping, v : Map[_, _]) =>
          def disjoint
            = v.view
                .map{case (k, v) => 
                  equaling(m.key, k) & notEqualing(m.value, v)
                }
                .reduceOption{_ | _}
                .asInstanceOf[Option[Select]]

          def notMatchingSize
            = havingCount(m, v.size, NotEqual)

          notMatchingSize ||! disjoint
      }

  //  or where false
  def everFalse ( m : Mapping ) : Statement = empty(m).copy(condition = Some(EverFalse))
  def everTrue ( m : Mapping ) : Statement = empty(m).copy(condition = Some(EverTrue))
}