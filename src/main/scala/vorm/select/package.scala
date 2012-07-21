package vorm

import extensions._
import query._
import structure._

package object select {
  
  sealed case class Statement
    ( sql : String,
      data : Seq[(Any, JdbcType)],
      resultSetColumns : Seq[(ddl.Column, mapping.Table)] )
  
  type JdbcType = Int


  def statement
    ( query : Query )
    : Statement
    = {

        sealed case class SelectNode
          // ( table : String,
          //   columns : List[String],
          //   mappings : Set[(String, String)],
          //   children : List[SelectNode] )

        def selectNode
          ( m : mapping.Table )
          : SelectNode
          = SelectNode(
              table
                = m.table.name,
              columns
                = m.table.columns.map( _ → m ),
              parentMappings
                = ???,
              children
                = m.subTableMappings.map( selectNode ),
              where
                = 

            )


        def statement
          ( selectNode : SelectNode )
          : Statement
          = ???


        statement( selectNode( query.mapping ) )
        
      }

    


}