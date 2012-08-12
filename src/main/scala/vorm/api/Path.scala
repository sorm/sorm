package vorm.api

import vorm._
import query._
import structure._
import mapping._
import extensions._

import Query._

object Path {

  trait Part
  object Part {
    case class Property ( name : String ) extends Part
    case class Key ( name : String ) extends Part
    case class Index ( index : Int ) extends Part
  }

  def parts
    ( p : String )
    : Stream[Part]
    = p.unfold{ _.notEmpty map partAndRemainder }

  def partAndRemainder
    ( p : String )
    : (Part, String)
    = """^(?:\.?(\w+)|\((\d+)\)|\((\w+)\))(.*)$""".r
        .findFirstMatchIn(p)
        .getOrElse {
          throw new Exception("Unparsable path: `" + p + "`")
        }
        .subgroups match {
          case List(property, null, null, remainder) =>
            ( Part.Property(property), remainder )
          case List(null, index, null, remainder) =>
            ( Part.Index(index.toInt), remainder )
          case List(null, null, key, remainder) =>
            ( Part.Key(key), remainder )
        }

  def where
    ( host : Mapping,
      path : String,
      value : Any,
      operator : Operator )
    : Where
    = where( host, parts(path), value, operator )

  private def where
    ( host : Mapping,
      path : Seq[Part],
      value : Any,
      operator : Operator )
    : Where
    = ( host, path ) match {
        case ( host : MapMapping, Part.Key( key ) +: tail ) =>
          And(
            Filter(Operator.Equals, host.key, key),
            where(host.value, tail, value, operator)
          )
        case ( host : SeqMapping, Part.Index( index ) +: tail ) =>
          And(
            Filter(Operator.Equals, host.index, index),
            where(host.item, tail, value, operator)
          )
        case ( host : EntityMapping, Part.Property( "id" ) +: Seq() ) =>
          Filter( operator, host.id, value )
        case ( host : EntityMapping, Part.Property(p) +: tail ) =>
          where(host.properties(p), tail, value, operator)
      }




  // def apply
  //   ( host : Mapping,
  //     path : Seq[Part.Part],
  //     value : Any,
  //     operator : Operator )
  //   : Where
  //   = ( host, path ) match {
  //       case ( _, Seq() ) =>
  //         Filter(operator, host, value)
  //       case ( host : MapMapping, Part.Key( key ) +: Seq() ) =>
  //         And(
  //           Filter(Operator.Equals, host.key, key),
  //           apply(host.value, path.tail, value, operator)
  //         )
  //       case ( host : MapMapping, 
  //              Part.Property("size") +: Seq() |
  //              Part.Property("keys") +: Part.Property("size") +: Seq() |
  //              Part.Property("values") +: Part.Property("size") +: Seq() ) =>
  //         Filter(Operator.HasSize, host, value)
  //       case ( host : MapMapping, Part.Property("keys") +: Seq() ) =>
  //         ( operator, value ) match {
  //           case (Operator.Equals, value : Traversable[_]) => 
  //             value.view
  //               .map{ Filter(Operator.Equals, host.key, _) }
  //               .reduceOption{ Or }
  //               .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
  //           case (Operator.Includes, value : Traversable[_]) 
  //             if value.size > 0 =>
  //             value.view
  //               .map{ Filter(Operator.Equals, host.key, _) }
  //               .reduce{ Or }
  //           case (Operator.Contains, value : Any) =>
  //             Filter(Operator.Equals, host.key, value)
  //         }

  //       case ( host : SeqMapping, Seq() ) =>
  //         ( operator, value ) match {
  //           case (Operator.Equals, value : Seq[_]) =>
  //             value.view
  //               .zipWithIndex
  //               .map{ case (v, i) => 
  //                 And(
  //                   Filter(Operator.Equals, host.item, v), 
  //                   Filter(Operator.Equals, host.index, i) 
  //                 )
  //               }
  //               .reduceOption{ Or }
  //               .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
  //         }
  //     }


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
