package vorm

import reflect.mirror

package object reflection {

  private val tpeCache =
    collection.mutable.Map[mirror.Type, Type]()

  private[reflection] def tpe[T](mt: mirror.Type): Type =
    try tpeCache(mt)
    catch {
      case _ =>
        val t = new Type(mt)
        tpeCache.update(mt, t)
        t
    }

  def tpe[T: TypeTag]: Type =
    tpe(tag[T].tpe)

  implicit def anyExtensions[T: TypeTag](x: T) = new AnyExtensions(x)
  /**
   * Seems like a bit too much
   */
  implicit def anyRefExtensions[T <: AnyRef : TypeTag](x: T) = new AnyRefExtensions(x)

}