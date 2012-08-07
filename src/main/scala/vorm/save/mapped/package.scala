package vorm.save

import vorm._
import structure._
import mapping._
import jdbc._
import sql._
import extensions._

package object mapped {

  trait Mapped {
    def value : Any
    def mapping : Mapping
  }
  object Mapped {
    def apply
      ( value : Any,
        mapping : Mapping )
      : Mapped
      = ???
  }
  trait MappedTableValue {
    def primaryKeyColumnValues : Seq[(String, JdbcValue)]
  }

  class MappedEntity
    ( val value : AnyRef,
      val mapping : EntityMapping )
    extends MappedTableValue
    {
      lazy val properties
        : Map[String, Mapped]
        = mapping.properties
            .map{ case (n, m) ⇒ 
              n → Mapped(mapping.reflection.propertyValue(n, value), m) 
            }
      lazy val isPersisted : Boolean
        = ???

    }
  
  class MappedTuple
    ( val value : Product,
      val mapping : TupleMapping )
    {
      def items
        = mapping.items
            .view
            .zipWithIndex
            .map{ case (m, i) ⇒ Mapped(value.productElement(i), m) }
    }
  
  class MappedSeq
    ( val value : Seq[_],
      val mapping : SeqMapping )
    {
      def items
        : Seq[Mapped]
        = value.map{ Mapped(_, mapping.item) }
    }




}