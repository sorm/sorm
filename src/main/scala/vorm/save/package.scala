package vorm

import vorm._
import structure._
import mapping._
import jdbc._
import sql._
import extensions._

package object save {

  /**
   * This implementation is based on strict rules about how statements are resolved to whether be Update or Insert. These are:
   * 1. All entities must have an autoincremented id property
   * 2. Whether this property is defined determines which method to use
   * 3. This may result in updates being called on inexisting rows - these situations should be considered caused by improper use and as such should result in a runtime exception being thrown
   * 4. All rows of collection tables are deleted before update and thus should result in no such conflicts
   */

  // def statements
  //   ( v : Any,
  //     m : EntityMapping )
  //   = {

  //     val values
  //       : Map[TableMapping, AnyRef]
  //       = {
  //         def childrenValues 
  //           ( v : Any,
  //             m : TableMapping )
  //           : Map[TableMapping, AnyRef]
  //           = m match {
  //               case m : EntityMapping =>
  //                 m.properties.map{ case (n, p) ⇒ 
  //                   childrenValues( m.reflection.propertyValue(v, n), p ) +
  //                   ( p)
  //                 }
  //             }

  //         childrenValues(v, m)
  //       }

  //     def rows
  //       ( v : Any,
  //         m : TableMapping )
  //       : Seq[Map[String, JdbcValue]]
  //       = 


  //     def allMappings : Seq[TableMapping]
  //     def value ( m : TableMapping ) : Any


  //     allMappings
  //       .orderedByDominance
  //       .

  //   }

  def statements
    ( v : Any,
      m : TableMapping )
    : Vector[Statement]
    = (v, m) match {
        case (v : Set[_], m : SetMapping) ⇒ 
          m.item match {
            case i : CollectionMapping ⇒ 
              v.flatMap{ v ⇒ rowStatement(v, m) +: statements(v, i) } 
            case _ ⇒ 
              v.flatMap{ v ⇒ statements(v, i) :+ rowStatement(v, i) }
          }
        case (v : Product, m : TupleMapping) ⇒ 
          m.items
            .view
            .zipWithIndex
            .flatMap{ case (m, i) ⇒ 
              statements( v.productElement(i), m )
            }
        case (_, _ : ValueMapping) ⇒ 
          Vector.empty
      }

////  shit, we need the generated columns

}