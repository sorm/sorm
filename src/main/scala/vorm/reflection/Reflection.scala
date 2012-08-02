package vorm.reflection

import reflect.mirror
import vorm._
import util.MurmurHash3

sealed class Reflection
  ( val t : mirror.Type )
  {
    lazy val javaClass
      = mirrorQuirks.javaClass(t)
    lazy val fullName
      = mirrorQuirks.fullName(t.typeSymbol)

    lazy val name
      = mirrorQuirks.name(t.typeSymbol)

    lazy val properties
      : Map[String, Reflection]
      = mirrorQuirks.properties(t).view
          .map {
            s ⇒ mirrorQuirks.name(s) → 
                Reflection(s.typeSignature)
          }
          .toMap

    lazy val generics
      : IndexedSeq[Reflection]
      = mirrorQuirks.generics(t).view
          .map(Reflection(_))
          .toIndexedSeq

    def inheritsFrom
      ( reflection : Reflection )
      : Boolean
      = reflection.fullName match {
          case n if n == "scala.Any" 
            ⇒ true
          case n if n == "scala.AnyVal" 
            ⇒ t.typeSymbol.isPrimitiveValueClass
          case _ 
            ⇒ reflection.javaClass.isAssignableFrom(javaClass) &&
              generics.view
                .zip(reflection.generics)
                .forall {case (a, b) => a.inheritsFrom(b)}
        }

    lazy val constructorArguments
      : Map[String, Reflection]
      = mirrorQuirks.constructors(t)
          .head
          .typeSignature
          .asInstanceOf[{def params: List[mirror.Symbol]}]
          .params
          .map(s ⇒ mirrorQuirks.name(s) → Reflection(s.typeSignature) )
          .toMap


    def instantiate
      ( params : Map[String, Any] )
      : Any
      = instantiate( constructorArguments.keys.map(params) )

    def instantiate
      ( args : Traversable[Any] = Nil )
      : Any
      = {
        if ( mirrorQuirks.isInner(t.typeSymbol) )
          throw new UnsupportedOperationException(
              "Dynamic instantiation of inner classes is not supported"
            )
        
        javaClass.getConstructors.head
          .newInstance( args.asInstanceOf[Seq[Object]] : _* )
      }
    override def equals(x: Any) =
      x match {
        case x: Reflection =>
          eq(x) ||
          t.typeSymbol == x.t.typeSymbol &&
          generics == x.generics
        case _ => super.equals(x)
      }
    override def hashCode =
      MurmurHash3.finalizeHash(t.typeSymbol.hashCode, generics.hashCode)
  }
  
object Reflection {

   val cache
    = new collection.mutable.HashMap[mirror.Type, Reflection] {
        override def default
          ( key : mirror.Type )
          = {
            val value = new Reflection(key)
            update(key, value)
            value
          }
      }

  def apply
    [ T ]
    ( implicit tag : TypeTag[T] )
    : Reflection
    = cache( tag.tpe )

  def apply
    ( mt : mirror.Type )
    : Reflection 
    = cache( mt )

}
