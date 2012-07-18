package vorm.structure

import vorm._
import reflection._

sealed trait ReflectionKind

object ReflectionKind {

  case object Entity
    extends ReflectionKind
  case object Value
    extends ReflectionKind
  case object Tuple
    extends ReflectionKind
  case object Option
    extends ReflectionKind
  case object Seq
    extends ReflectionKind
  case object Set
    extends ReflectionKind
  case object Map
    extends ReflectionKind

  def apply
    ( reflection : Reflection )
    = reflection match {
        case _
          if reflection inheritsFrom reflectionOf[collection.Seq[_]]
          ⇒ Seq
        case _
          if reflection inheritsFrom reflectionOf[collection.Set[_]]
          ⇒ Set
        case _
          if reflection inheritsFrom reflectionOf[collection.Map[_, _]]
          ⇒ Map
        case _
          if reflection inheritsFrom reflectionOf[scala.Option[_]]
          ⇒ Option
        case _
          if (reflection inheritsFrom reflectionOf[AnyVal])
          || (reflection inheritsFrom reflectionOf[String])
          || (reflection inheritsFrom reflectionOf[org.joda.time.DateTime])
          ⇒ Value
        case _
          if (reflection inheritsFrom reflectionOf[Tuple1[_]])
          || (reflection inheritsFrom reflectionOf[Tuple2[_, _]])
          || (reflection inheritsFrom reflectionOf[Tuple3[_, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple4[_, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple5[_, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple6[_, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple7[_, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple8[_, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple9[_, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple10[_, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          || (reflection inheritsFrom reflectionOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
//          || (reflection inheritsFrom reflectionOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
//          || (reflection inheritsFrom reflectionOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
//          || (reflection inheritsFrom reflectionOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
//          || (reflection inheritsFrom reflectionOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]])
          ⇒ Tuple
        case _
          ⇒ Entity
      }
}