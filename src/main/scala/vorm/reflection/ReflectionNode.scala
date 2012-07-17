package vorm.reflection

sealed trait ReflectionNode {
  def reflection : Reflection
  def generics
    = reflection.generics.view
        .zipWithIndex
        .map { case (r, i) ⇒ new ReflectionNode.Generic(i, r, this) }
        .toIndexedSeq
  def properties
    = reflection.properties
        .map { case (n, r) ⇒ new ReflectionNode.Property(n, r, this) }
}

object ReflectionNode {

  def apply
    ( reflection : Reflection )
    = new Root (reflection)

  sealed case class Root
    ( reflection : Reflection )
    extends ReflectionNode

  sealed trait Child
    extends ReflectionNode
    {
      def parent : ReflectionNode
      def root 
        : Root
        = parent match {
            case parent : Child ⇒ parent.root
            case parent : Root  ⇒ parent
          }

    }

  sealed case class Generic
    ( index : Int, 
      reflection : Reflection, 
      parent : ReflectionNode )
    extends Child

  sealed case class Property
    ( name : String,
      reflection : Reflection,
      parent : ReflectionNode )
    extends Child
}