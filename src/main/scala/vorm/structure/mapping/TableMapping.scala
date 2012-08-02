package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

trait TableMapping
  extends Mapping
  {
//    def columns : Seq[Column]

    protected def subColumns
      ( m : Mapping )
      : Seq[Column]
      = m match {
          case m : ValueMapping =>
            m.column :: Nil
          case m : TupleMapping =>
            m.items flatMap subColumns
          case m : OptionMapping =>
            subColumns( m.item )
          case m : TableMapping =>
            Nil
        }

    def primaryKeyColumns : Seq[Column]

  }
