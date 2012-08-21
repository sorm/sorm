package vorm.sql

import vorm._
import extensions._

object NewSql {
  
  sealed trait Sql

  sealed trait Statement 
    extends Sql

  sealed case class Union 
    [ L <: Statement,
      R <: Statement ]
    ( left : L,  
      right : R ) 
    extends Statement

//  sealed case class Select
//    ( what : Seq[SelectWhat],
//      from : From,
//      join : Seq[Join] = Nil,
//      where : Option[Clause] )
//    extends Statement


}