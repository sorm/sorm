package vorm

import vorm._
import structure._
import mapping._
import jdbc._
import sql._
import save._
import mapped._
import extensions._

/**
 * This implementation is based on strict rules about how statements are resolved to whether be Update or Insert. These are:
 * 1. All entities must have an autoincremented id property
 * 2. Whether this property is defined determines which method to use
 * 3. This may result in updates being called on inexisting rows - these situations should be considered caused by improper use and as such should result in a runtime exception being thrown
 * 4. All rows of collection tables are deleted before update and thus should result in no such conflicts
 */
package object save {

  trait SaveAdapter extends ConnectionAdapter {
    def saveEntityAndGetIt
      ( e : MappedEntity )
      : AnyRef
      = {
        // val values
        //   = 

        // e.mapping.reflection.properties.keys.view

        if( e.isPersisted ){
          ???
        } else {
          val stmt : Statement
            = ???

          val id : Long
            = executeUpdateAndGetGeneratedKeys( stmt )
                .head.head.asInstanceOf[Long]
          
          // var primaryKey = IndexedSeq[Any]()

          // executeUpdateAndGetGeneratedKeys(stmt)
          //   .tap{ primaryKey = _.head }

        }
      }
  }

  // implicit class MappingResultSetParsingAdapter
  //   ( m : Mapping )
  //   {
  //     def propertyValues 
  //       ( rs : ResultSet, 
  //         columns : Seq[Column] ) 
  //       : Seq[Map[String, Any]]
  //   }
  
}