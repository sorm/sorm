package vorm.query

import vorm._
import reflection._
import structure._

case class Query
  ( mapping : Mapping,
    where   : Option[WhereNode] = None,
    order   : Seq[Order]        = Nil,
    limit   : Limit             = Limit() )
