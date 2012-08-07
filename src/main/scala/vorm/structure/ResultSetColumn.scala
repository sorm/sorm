package vorm.structure

import mapping._

/**
 * Just an id
 */
sealed case class ResultSetColumn
  ( name : String, 
    jdbcType : Int, 
    mapping : TableMapping )
