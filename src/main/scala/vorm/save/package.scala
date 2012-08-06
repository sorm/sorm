package vorm

import vorm._
import structure._
import mapping._
import jdbc._
import sql._
import extensions._

package object save {

  // class Mapped
  //   ( val value : Any,
  //     val mapping : Mapping )
  //   {
  //     def columnValues = ???

  //     def children
  //       = mapping.children

  //   }

  // case class SaveQuery
  //   ( value : Any,
  //     mapping : Mapping )
  //   {
  //   }

  // class MappedEntity
  //   ( val value : AnyRef,
  //     val mapping : EntityMapping )
  //   {
  //     lazy val properties
  //       = mapping.properties
  //           .map{ case (n, p) ⇒ Mapped( mapping.reflection.propertyValue(n), p ) }
  //   }


  /**
   * This implementation is based on strict rules about how statements are resolved to whether be Update or Insert. These are:
   * 1. All entities must have an autoincremented id property
   * 2. Whether this property is defined determines which method to use
   * 3. This may result in updates being called on inexisting rows - these situations should be considered caused by improper use and as such should result in a runtime exception being thrown
   * 4. All rows of collection tables are deleted before update and thus should result in no such conflicts
   */

  def statements
    ( v : Any,
      m : EntityMapping )
    = {
      // def columnValues
      //   ( m : Mapping )

      // val rows 
      //   : Map[TableMapping, Seq[Map[String, JdbcValue]]]
      //   = 
      lazy val isPersisted : Boolean
        = ???

      val values
        : Map[TableMapping, AnyRef]
        = ???

      def rows
        ( v : Any,
          m : TableMapping )
        : Seq[Map[String, JdbcValue]]
        = 


      def allMappings : Seq[TableMapping]
      def value ( m : TableMapping ) : Any


      allMappings
        .orderedByDominance
        .

    }




}