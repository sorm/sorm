package sorm.core

import sorm._
import query._
import structure._
import mapping._
import sext.Sext._

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
          throw new SormException("Unparsable path: `" + p + "`")
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

  private def where
    ( host : Mapping,
      path : Seq[Part],
      value : Any,
      operator : Operator )
    : Where
    = ( host, path ) match {
        case (_, Seq()) =>
          Filter(operator, host, value)
        case (host : MapMapping, Part.Braced(key) +: tail) =>
          And(
            Filter(Operator.Equal, host.key, key),
            where(host.value, tail, value, operator)
          )
        case (host : SeqMapping, Part.Braced(index) +: tail) =>
          And(
            Filter(Operator.Equal, host.index, index.toInt),
            where(host.item, tail, value, operator)
          )
        //  virtual properties should not be supported until a general api is developed
        // case (host : CollectionMapping, Part.Dotted("size") +: Seq()) =>
        //   ( operator, value ) match {
        //     case (Operator.Equal, _) =>
        //       Filter(Operator.HasSize, host, value)
        //   }
        // case (host : MapMapping, Part.Dotted("keys") +: Seq()) =>
        //   ( operator, value ) match {
        //     case (Operator.Equal, value : Traversable[_]) =>
        //       value.view
        //         .map{ Filter(Operator.Equal, host.key, _) }
        //         .reduceOption{ Or }
        //         .foldLeft( Filter(Operator.HasSize, host, value.size) ){ And }
        //     case (Operator.Includes, value : Traversable[_]) 
        //       if value.size > 0 =>
        //       value.view
        //         .map{ Filter(Operator.Equal, host.key, _) }
        //         .reduce{ Or }
        //     case (Operator.Contains, value : Any) =>
        //       Filter(Operator.Equal, host.key, value)
        //   }         
        case (_, Part.Dotted(id) +: tail) =>
          where( mapping(host, id), tail, value, operator )
      }


  def mapping
    ( host : Mapping,
      path : String )
    : Mapping
    = if( path == "" ) host
      else ( host, path.splitBy(".") ) match {
        case (host : EntityMapping, ("id", r)) =>
          mapping( host.id, r )
        case (host : EntityMapping, (id, r)) =>
          mapping( host.properties(id), r )
        case (host : TupleMapping, (id, remainder)) =>
          "(?<=^_)\\d+(?=$)".r.findFirstIn(id) match {
            case Some(index) =>
              mapping( host.items(index.toInt - 1), remainder )
            case None =>
              throw new SormException("Unparseable tuple item id `" + id + "` in path `" + path + "` of `" + host + "`")
          }
        case (host : OptionMapping, ("item", remainder)) =>
          mapping( host.item, remainder )
        case (host : SeqMapping, ("item", remainder)) =>
          mapping( host.item, remainder )
        case (host : SetMapping, ("item", remainder)) =>
          mapping( host.item, remainder )
        case (host : MapMapping, ("key", remainder)) =>
          mapping( host.key, remainder )
        case (host : MapMapping, ("value", remainder)) =>
          mapping( host.value, remainder )
        case (_, (id, _)) =>
          throw new SormException("Unparseable id `" + id + "` in path `" + path + "` of `" + host + "`")
      }

}
