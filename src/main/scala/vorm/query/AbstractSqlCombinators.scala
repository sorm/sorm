package vorm.query

import vorm.structure.mapping._
import vorm.persisted._
import vorm.extensions._

import vorm.abstractSql.AbstractSql._
import vorm.abstractSql.Compositing._

object AbstractSqlCombinators {

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
        havingCount
          = Some(
              HavingCount(
                m.abstractSqlTable,
                m.primaryKeyColumns.last.name,
                o,
                v
              )
            )
      )
  def havingCount ( m : TableMapping, v : Int, o : Operator = Equal ) : Select
    = restrictingCount( empty(m), m, v, o )

  def empty ( m : Mapping ) : Select
    = m.root.abstractSqlPrimaryKeySelect

  def havingNotEmptyContainer ( m : Mapping ) : Option[Select]
    = m.containerTableMapping.map{ havingCount(_, 0, NotEqual) }

  def including ( m : CollectionMapping, v : Iterable[_] ) : Option[Statement]
    = {
      val item
        = m match {
            case m : SeqMapping => m.item
            case m : SetMapping => m.item
            case m : MapMapping => m.key
          }
      v.view
        .map{ equaling(item, _) }
        .reduceOption{ union }
        .asInstanceOf[Option[Select]]
        .map{ restrictingCount(_, m, v.size) }
    }


  def equaling ( m : Mapping, v : Any ) : Statement
    = (m, v) match {
        case (m : ValueMapping, v) =>
          m.root.abstractSqlPrimaryKeySelect
            .copy(
              condition
                = Some(
                    Comparison(
                      m.containerTableMapping.get.abstractSqlTable,
                      m.columnName,
                      Equal,
                      v
                    )
                  )
            )
        case (m : EntityMapping, v : Persisted) =>
          equaling(m.id, v.id)
        case (m : TupleMapping, v : Product ) =>
          v.productIterator.zipWithIndex
            .map{ case (v, i) => equaling(m.items(i), v) }
            .reduce{_ & _}
        case (m : OptionMapping, v : Option[_] ) =>
          equaling(m.item, v.orNull)
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
        case (m : ValueMapping, v) =>
          empty(m).copy(
            condition
              = Some(
                  Comparison(
                    m.containerTableMapping.get.abstractSqlTable,
                    m.columnName,
                    NotEqual,
                    v
                  )
                )
          )
        case (m : EntityMapping, v : Persisted) =>
          notEqualing(m.id, v.id)
        case (m : TupleMapping, v : Product ) =>
          v.productIterator.zipWithIndex
            .map{ case (v, i) => notEqualing(m.items(i), v) }
            .reduce{_ | _}
        case (m : OptionMapping, v : Option[_] ) =>
          notEqualing(m.item, v.orNull)
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

}