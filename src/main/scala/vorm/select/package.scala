package vorm

import extensions._
import query._
import structure._
import reflection._

package object select {
  
  sealed case class Statement
    ( template : String,
      values : Seq[(Any, JdbcType)],
      resultSetColumns : Seq[(ddl.Column, mapping.Table)] )
  
  type JdbcType = Int


  def statement
    ( q : Query )
    : Statement
    = {

      // val aliases
      //   = {
      //     def aliases
      //       ( m : mapping.Table, 
      //         acc : Map[mapping.Table, String] )
      //       : Map[mapping.Table, String]
      //       = m.subTableMappings
      //           .foldRight( acc + (m → ("t" + acc.size) ))( aliases )
      //     aliases( query.mapping, Map() )
      //   }

      val alias
        = new collection.mutable.HashMap[mapping.Table, String]() {
            override def default
              ( m : mapping.Table )
              = {
                val n = "t" + size
                update(m, n)
                n
              }

          }


      // def and
      //   ( items : Seq[WhereNode] )
      //   // = items match {
      //   //   case x :: xs ⇒ sql.WhereNode.And(x, and(xs)) 
      //   // }
      //   = items.tail.foldRight(items.head)(sql.WhereNode.And)



      // approximately like that
      // case class SqlClausesNode
      //   ( where : _,
      //     groups : _ )

      def where
        ( n : query.WhereNode )
        : sql.WhereNode
        = n match {
            case query.WhereNode.And(left, right)
              ⇒ sql.WhereNode.And( where(left), where(right) )
            case query.WhereNode.Or(left, right)
              ⇒ sql.WhereNode.Or( where(left), where(right) )

            case query.WhereNode.Equals( m : mapping.Value, v )
              ⇒ sql.WhereNode.Equals(
                  alias(m.parentTableMapping) + "." + m.column.name,
                  (v, m.column.t.jdbcType)
                )
            case query.WhereNode.Equals( m : mapping.Tuple, v : Product ) 
              ⇒ m.children
                  .view
                  .map( m ⇒ m.child -> m.index )
                  .map{ 
                    case (m, i) 
                      ⇒ query.WhereNode.Equals( m, v.productElement(i) ) 
                  }
                  .map( where )
                  .reduce( sql.WhereNode.And )
            case query.WhereNode.Equals( m : mapping.Option, v : Option[_] )
              ⇒ v match {
                  case Some(v) 
                    ⇒ where( query.WhereNode.Equals(m.child.child, v) )
                  case None
                    ⇒ where( query.WhereNode.Equals(m.child.child, null) )
                }
            case query.WhereNode.Equals( m : mapping.)
          }



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