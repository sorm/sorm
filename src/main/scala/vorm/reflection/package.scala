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

  implicit def anyExtensions[T: TypeTag](x: T) = new AnyExtensions(x)
  /**
   * Seems like a bit too much
   */
  implicit def anyRefExtensions[T <: AnyRef : TypeTag](x: T) = new AnyRefExtensions(x)

}