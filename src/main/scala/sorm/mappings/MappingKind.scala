package sorm.mappings

import sext._, embrace._
import sorm._
import core.SormException
import reflection._

sealed trait MappingKind

object MappingKind {

  case object Enum             extends MappingKind
  case object Entity           extends MappingKind
  case object Value            extends MappingKind
  case object Tuple            extends MappingKind
  case object OptionToTable    extends MappingKind
  case object OptionToNullable extends MappingKind
  case object Seq              extends MappingKind
  case object Set              extends MappingKind
  case object Map              extends MappingKind
  case object Range            extends MappingKind

  def apply
    ( reflection : Reflection )
    : MappingKind
    = reflection match {
        case _
          if reflection <:< Reflection[scala.Range]
          => Range
        case _
          if reflection <:< Reflection[collection.Seq[_]]
          => Seq
        case _
          if reflection <:< Reflection[collection.Set[_]]
          => Set
        case _
          if reflection <:< Reflection[collection.Map[_, _]]
          => Map
        case _
          if reflection <:< Reflection[scala.Option[_]]
          =>
          if( MappingKind(reflection.generics(0)) $ scala.Seq(Seq, Set, Map, OptionToTable, OptionToNullable).contains )
            OptionToTable
          else
            OptionToNullable
        case _
          if (reflection <:< Reflection[AnyVal])
          || (reflection <:< Reflection[String])
          || (reflection <:< Reflection[BigDecimal])
          || (reflection <:< Reflection[org.joda.time.DateTime])
          || (reflection <:< Reflection[org.joda.time.LocalDate])
          || (reflection <:< Reflection[org.joda.time.LocalTime])
          => Value
        case _
          if (reflection <:< Reflection[Enumeration#Value])
          => Enum
        case _
          if (reflection <:< Reflection[Tuple1[_]])
          || (reflection <:< Reflection[Tuple2[_, _]])
          || (reflection <:< Reflection[Tuple3[_, _, _]])
          || (reflection <:< Reflection[Tuple4[_, _, _, _]])
          || (reflection <:< Reflection[Tuple5[_, _, _, _, _]])
          || (reflection <:< Reflection[Tuple6[_, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple7[_, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple8[_, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple9[_, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple10[_, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple11[_, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection <:< Reflection[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          => Tuple
        case _
          if reflection.isCaseClass
          => Entity
        case _
          => throw new SormException("Unsupported type: " + reflection)
      }
}