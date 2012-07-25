package vorm.structure

/**
 * Just an id
 */
sealed case class ResultSetColumn
  ( name : String, 
    jdbcType : Int, 
    mapping : Mapping )
