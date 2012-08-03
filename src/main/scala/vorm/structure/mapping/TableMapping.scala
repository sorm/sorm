package vorm.structure.mapping

import vorm._
import reflection._
import ddl._
import select._
import structure._
import extensions._

trait TableMapping
  extends Mapping
  {


//    def columns : Seq[Column]

    def primaryKeyColumns : Seq[Column]

    def foreignKeyForOwnerTable : Option[ForeignKey]

    def ownerTableForeignKey : Option[ForeignKey]


    def children : Iterable[Mapping]

    lazy val subTableMappings : Iterable[TableMapping] 
      = {
        def nestedTableMappings
          ( m : Mapping )
          : Seq[TableMapping]
          = m match {
            case m : ValueMapping ⇒ Nil
            case m : TupleMapping ⇒ m.items flatMap nestedTableMappings
            case m : OptionMapping ⇒ m.item as nestedTableMappings
            case m : TableMapping ⇒ m :: Nil
          }
        children flatMap nestedTableMappings
      }

    protected def subColumns
      ( m : Mapping )
      : Seq[Column]
      = m match {
          case m : ValueMapping ⇒ m.column :: Nil
          case m : TupleMapping ⇒ m.items flatMap subColumns
          case m : OptionMapping ⇒ subColumns( m.item )
          case m : TableMapping ⇒ 
            m.primaryKeyColumns.map{ c ⇒ 
              c.copy(
                name = m.columnName + "$" + c.name,
                autoIncremented = false
              ) 
            }
        }

    lazy val valueColumns : Iterable[Column] 
      = children flatMap subColumns

  }
