package vorm.query

import vorm._
import reflection._

case class Query
  ( mapping     : Mapping,
    filter      : Filter      = Nil,
    orders      : Seq[Order]  = Nil,
    limit       : Limit       = Limit() )
