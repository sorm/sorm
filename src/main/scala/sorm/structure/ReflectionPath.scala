package sorm.structure

/**
 * Just for pointing to location in some reflection
 */
sealed trait ReflectionPath
object ReflectionPath {

  object Root
    extends ReflectionPath

  sealed trait Child
    extends ReflectionPath
    {
      def parent : ReflectionPath
    }

  sealed case class EntityProperty
    ( parent  : ReflectionPath,
      name    : String )
    extends Child

  sealed case class SeqItem
    ( parent : ReflectionPath )
    extends Child

  sealed case class SetItem
    ( parent : ReflectionPath )
    extends Child

  sealed case class MapKey
    ( parent : ReflectionPath )
    extends Child

  sealed case class MapValue
    ( parent : ReflectionPath )
    extends Child

  sealed case class TuplePart
    ( parent : ReflectionPath,
      index  : Int )
    extends Child

}
