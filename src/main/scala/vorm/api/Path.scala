package vorm.api

import vorm._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import query._
import select._
import resultSet._
import extensions._

import Query._

import collection.immutable.Queue

object Path {

  def mapping
    ( parent : Mapping,
      path : String )
    : Mapping
    = ( parent, path ) match {
        case (_, "") =>
          parent
        case (parent : EntityMapping, _) =>
          path.splitBy(".") match {
            case ("id", remainder) =>
              remainder match {
                case "" => 
                  parent.id
              }
            case (property, remainder) =>
              mapping(parent.properties(property), remainder)
          }
        case (parent : TupleMapping, _) =>
          path.splitBy(".") match {
            case (item, remainder) =>
              "(?<=^_)\\d+(?=$)".r.findFirstIn(item) match {
                case Some(index) =>
                  mapping( parent.items(index.toInt - 1), remainder )
              }
          }
        case (parent : OptionMapping, _) =>
          mapping( parent.item, path )
        case (parent : SeqMapping, _) =>
          mapping( parent.item, path )
        case (parent : SetMapping, _) =>
          mapping( parent.item, path )
        case (parent : MapMapping, _ ) =>
          path.splitBy(".") match {
            case ("key", remainder) =>
              mapping( parent.key, remainder )
            case ("value", remainder) =>
              mapping( parent.value, remainder )
          }
      }

}
