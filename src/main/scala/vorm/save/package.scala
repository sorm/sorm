package vorm

import vorm._
import structure._
import mapping._
import jdbc._
import sql._
import extensions._

/**
 * This implementation is based on strict rules about how statements are resolved to whether be Update or Insert. These are:
 * 1. All entities must have an autoincremented id property
 * 2. Whether this property is defined determines which method to use
 * 3. This may result in updates being called on inexisting rows - these situations should be considered caused by improper use and as such should result in a runtime exception being thrown
 * 4. All rows of collection tables are deleted before update and thus should result in no such conflicts
 */
package object save {

  trait Saver {
    def connection : Connection 
    def saveEntityAndGetIt
      ( e : MappedEntity )
      : AnyRef
      = ???
  }
  
}