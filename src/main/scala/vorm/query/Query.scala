package vorm.query

import vorm._
import reflection._

case class Query
  ( mapping : Mapping,
    filters : FilterNode  = Nil,
    orders  : Seq[Order]  = Nil,
    limit   : Limit       = Limit() )
