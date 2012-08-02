package vorm.structure

import vorm._
import structure._
import reflection._
import mapping._
import mapping.{
  Table ⇒ TableMapping,
  Value ⇒ ValueMapping,
  Tuple ⇒ TupleMapping,
  Seq ⇒ SeqMapping,
  Set ⇒ SetMapping,
  Map ⇒ MapMapping,
  Entity ⇒ EntityMapping,
  Option ⇒ OptionMapping
}

trait Mapping {
  def settings : Settings
  def parentTableMapping : scala.Option[Table]
}
object Mapping {
  /**
   * Root
   */
  def apply
    ( settings : Settings )
    = new Root( settings )
    
  def apply
    ( reflection : Reflection,
      parent : Mapping,
      settings : Settings )
    : Mapping
    = ReflectionKind (reflection) match {
        case ReflectionKind.Value
          ⇒ new ValueMapping (reflection, parent, settings)
        case ReflectionKind.Tuple
          ⇒ new TupleMapping (reflection, parent, settings)
        case ReflectionKind.Seq
          ⇒ new SeqMapping (reflection, parent, settings)
        case ReflectionKind.Set
          ⇒ new SetMapping (reflection, parent, settings)
        case ReflectionKind.Map
          ⇒ new MapMapping (reflection, parent, settings)
        case ReflectionKind.Entity
          ⇒ new EntityMapping (reflection, parent, settings)
        case ReflectionKind.Option
          ⇒ new OptionMapping (reflection, parent, settings)
      }
}