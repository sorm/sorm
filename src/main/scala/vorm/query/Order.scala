package vorm.query

import vorm._
import reflection._
import structure._

case class Order
  ( mapping : Mapping,
    reverse : Boolean = false )