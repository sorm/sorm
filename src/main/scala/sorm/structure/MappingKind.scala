package sorm.structure

import sorm._
import reflection._

sealed trait MappingKind

object MappingKind {

  case object Enum   extends MappingKind
  case object Entity extends MappingKind
  case object Value  extends MappingKind
  case object Tuple  extends MappingKind
  case object Option extends MappingKind
  case object Seq    extends MappingKind
  case object Set    extends MappingKind
  case object Map    extends MappingKind
  case object Range  extends MappingKind

  def apply
    ( reflection : Reflection )
    = reflection match {
        case _
          if reflection inheritsFrom Reflection[scala.Range]
          => Range
        case _
          if reflection inheritsFrom Reflection[collection.Seq[_]]
          => Seq
        case _
          if reflection inheritsFrom Reflection[collection.Set[_]]
          => Set
        case _
          if reflection inheritsFrom Reflection[collection.Map[_, _]]
          => Map
        case _
          if reflection inheritsFrom Reflection[scala.Option[_]]
          => Option
        case _
          if (reflection inheritsFrom Reflection[AnyVal])
          || (reflection inheritsFrom Reflection[String])
          || (reflection inheritsFrom Reflection[org.joda.time.DateTime])
          || (reflection inheritsFrom Reflection[org.joda.time.LocalDate])
          || (reflection inheritsFrom Reflection[org.joda.time.LocalTime])
          => Value
        case _
          if (reflection inheritsFrom Reflection[Enumeration#Value])
          => Enum
        case _
          if (reflection inheritsFrom Reflection[Tuple1[_]])
          || (reflection inheritsFrom Reflection[Tuple2[_, _]])
          || (reflection inheritsFrom Reflection[Tuple3[_, _, _]])
          || (reflection inheritsFrom Reflection[Tuple4[_, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple5[_, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple6[_, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple7[_, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple8[_, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple9[_, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple10[_, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple11[_, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom Reflection[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          => Tuple
        case _
          => Entity
      }
}