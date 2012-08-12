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
    case class Dotted ( name : String ) extends Part
    case class Braced ( name : String ) extends Part
  }

  def parts
    ( p : String )
    : Stream[Part]
    = p.unfold{ _.notEmpty map partAndRemainder }

  def partAndRemainder
    ( p : String )
    : (Part, String)
    = """^(?:\.?(\w+)|\((\w+)\))(.*)$""".r
        .findFirstMatchIn(p)
        .getOrElse {
          throw new Exception("Unparsable path: `" + p + "`")
        }
        .subgroups match {
          case List(name, null, remainder) =>
            ( Part.Dotted(name), remainder )
          case List(null, name, remainder) =>
            ( Part.Braced(name), remainder )
        }

  def where
    ( host : Mapping,
      path : String,
      value : Any,
      operator : Operator )
    : Where
    = where( host, parts(path), value, operator )
    // = ( host, partAndRemainder(path) ) match {
    //     case (host : MapMapping, (Part.Braced(k), p)) =>
    //       And(
    //         Filter(Operator.Equals, host.key, k),
    //         where(host.value, p, value, operator)
    //       )
    //     case (host : SeqMapping, (Part.Index(i), p)) =>
    //       And(
    //         Filter(Operator.Equals, host.index, i),
    //         where(host.item, p, value, operator)
    //       )
    //   }

  private def where
    ( host : Mapping,
      path : Seq[Part],
      value : Any,
      operator : Operator )
    : Where
    = ( host, path ) match {
        case ( _, Seq() ) =>
          Filter(operator, host, value)
        case ( host : MapMapping, Part.Braced(key) +: tail ) =>
          And(
            Filter(Operator.Equals, host.key, key),
            where(host.value, tail, value, operator)
          )
        case ( host : SeqMapping, Part.Braced(index) +: tail ) =>
          And(
            Filter(Operator.Equals, host.index, index.toInt),
            where(host.item, tail, value, operator)
          )
        case ( host : EntityMapping, Part.Dotted("id") +: Seq() ) =>
          Filter( operator, host.id, value )
        case ( host : EntityMapping, Part.Dotted(p) +: tail ) =>
          where(host.properties(p), tail, value, operator)
        case ( host : TupleMapping, Part.Dotted(p) ) =>
          ???
      }

  private def mappingAndRemainder
    ( host : Mapping,
      path : String )
    : (Mapping, String)
    = if( path == "" ) ( host, "" )
      else ???


  // def apply
  //   ( host : Mapping,
  //     path : Seq[Part.Part],
  //     value : Any,
  //     operator : Operator )
  //   : Where
  //   = ( host, path ) match {
  //       case ( _, Seq() ) =>
  //         Filter(operator, host, value)
  //       case ( host : MapMapping, Part.Braced( key ) +: Seq() ) =>
  //         And(
  //           Filter(Operator.Equals, host.key, key),
  //           apply(host.value, path.tail, value, operator)
  //         )
  //       case ( host : MapMapping, 
  //              Part.Dotted("size") +: Seq() |
  //              Part.Dotted("keys") +: Part.Dotted("size") +: Seq() |
  //              Part.Dotted("values") +: Part.Dotted("size") +: Seq() ) =>
  //         Filter(Operator.HasSize, host, value)
  //       case ( host : MapMapping, Part.Dotted("keys") +: Seq() ) =>
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
