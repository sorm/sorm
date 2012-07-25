package vorm.selectAbstraction

import vorm._
import extensions._
import structure._
import query._
import selectAbstraction._

object Composing {


  case class Statement
    ( sql : sql.Select, 
      // results : Seq[(ddl.Column, mapping.Table)] )
      results : Seq[ResultSetColumn] )


  // def statement
  //   ( s : Select )
  //   : Statement
  //   = n match {
  //       case c : 
  //         ⇒ 
  //     }

  // def withNode
  //   ( s : Statement,
  //     n : Clause )
  //   : Statement
  //   = n match {
  //       case Clause.And( left, right )
  //         ⇒ ???
  //       case Clause.Select( t, m, c, None )
  //         ⇒ ???
  //       case n : Clause.Select
  //         ⇒ ???
  //     }


  def andNode
    ( s : Statement,
      c : Clause )
    : Statement
    = {
      lazy val alias = "t" + (s.sql.join.length + 1)
      c match {
        case c : Clause.Select
          ⇒ s.copy(
                sql
                  = s.sql.copy(
                        join
                          = s.sql.join :+
                            sql.Join(
                                what = pkSelect(c),
                                as = alias
                              )
                        where
                          = c.mapping.pkColumns.view
                              .map( _.name )
                              .map( sql.Column(_, Some(alias)) )
                              .map( sql.Clause.Equals(_) )
                              .foldFrom( s.sql.where ) 


                          // = sql.Clause.And(
                          //       s.sql.where,
                          //       sql.Clause.Equals
                          //     )
                      )
              )
      }
    }

    


}