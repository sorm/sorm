package vorm.query

import vorm._
import reflection._

case class Order
  ( mapping : Mapping,
    reverse : Boolean = false )