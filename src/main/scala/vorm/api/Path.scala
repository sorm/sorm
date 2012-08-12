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
    = if( path == "" ) parent
      else try parent match {
        case parent : EntityMapping =>
          path.splitBy(".") match {
            case ("id", remainder) =>
              remainder match {
                case "" =>
                  parent.id
              }
            case (property, remainder) =>
              mapping(parent.properties(property), remainder)
          }
        case parent : TupleMapping =>
          path.splitBy(".") match {
            case (item, remainder) =>
              "(?<=^_)\\d+(?=$)".r.findFirstIn(item) match {
                case Some(index) =>
                  mapping( parent.items(index.toInt - 1), remainder )
              }
          }
        case parent : OptionMapping =>
          mapping( parent.item, path )
        case parent : SeqMapping =>
          path.splitBy(".") match {
            case ("item", remainder) =>
              mapping( parent.item, remainder )
          }
        case parent : SetMapping =>
          path.splitBy(".") match {
            case ("item", remainder) =>
              mapping( parent.item, remainder )
          }
        case parent : MapMapping =>
          path.splitBy(".") match {
            case ("key", remainder) =>
              mapping( parent.key, remainder )
            case ("value", remainder) =>
              mapping( parent.value, remainder )
          }
      } catch {
        case e : MatchError =>
          throw new Exception("Unparseable path `" + path + "` of `" + parent + "`", e)
      }

}
