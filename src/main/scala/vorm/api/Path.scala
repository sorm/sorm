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
    ( host : Mapping,
      path : String )
    : Mapping
    = if( path == "" ) host
      else try host match {
        case host : EntityMapping =>
          path.splitBy(".") match {
            case ("id", remainder) =>
              remainder match {
                case "" =>
                  host.id
              }
            case (property, remainder) =>
              mapping(host.properties(property), remainder)
          }
        case host : TupleMapping =>
          path.splitBy(".") match {
            case (item, remainder) =>
              "(?<=^_)\\d+(?=$)".r.findFirstIn(item) match {
                case Some(index) =>
                  mapping( host.items(index.toInt - 1), remainder )
              }
          }
        case host : OptionMapping =>
          mapping( host.item, path )
        case host : SeqMapping =>
          path.splitBy(".") match {
            case ("item", remainder) =>
              mapping( host.item, remainder )
          }
        case host : SetMapping =>
          path.splitBy(".") match {
            case ("item", remainder) =>
              mapping( host.item, remainder )
          }
        case host : MapMapping =>
          path.splitBy(".") match {
            case ("key", remainder) =>
              mapping( host.key, remainder )
            case ("value", remainder) =>
              mapping( host.value, remainder )
          }
      } catch {
        case e : MatchError =>
          throw new Exception("Unparseable path `" + path + "` of `" + host + "`", e)
      }

}
