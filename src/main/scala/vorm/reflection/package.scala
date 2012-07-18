package vorm

import reflect.mirror

package object reflection {

  private val tpeCache =
    collection.mutable.Map[mirror.Type, Type]()

  private[reflection] def tpe[T](mt: mirror.Type, jc: Option[Class[_]] = None): Type =
    try tpeCache(mt)
    catch {
      case _ =>
        val t = new Type(mt, jc)
        tpeCache.update(mt, t)
        t
    }

  def tpe[T: TypeTag]: Type =
    tag[T] match { case t => tpe(t.tpe, Some(t.erasure)) }



  private val reflectionCache =
    new collection.mutable.HashMap[(mirror.Type, Class[_]), Reflection]() {
      override def default(key: (mirror.Type, Class[_])) = {
        val value = new Reflection(key._1, key._2)
        update(key, value)
        value
      }
    }

  def reflectionOf(mt: mirror.Type): Reflection =
    reflectionCache(mt, mirrorQuirks.javaClass(mt))

  // todo : to move to apply of Reflection
  def reflectionOf[T](implicit t: TypeTag[T]): Reflection =
    reflectionCache(t.tpe, t.erasure)


  implicit class AnyReflectedSupport
    [ T : TypeTag ]
    ( any : T )
    {
      def reflected
        = new Reflected( any, Reflection( typeTag[T] ) )
    }



}