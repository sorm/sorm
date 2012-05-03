package vorm

import reflect.mirror

package object reflection {

  private val tpeCache =
    collection.mutable.Map[mirror.Type, Type]()

  private[reflection] def tpe[T](mt: mirror.Type): Type =
    try tpeCache(mt)
    catch {
      case _ => {
        val t = new Type(mt)
        tpeCache.update(mt, t)
        t
      }
    }

  def reflection[T: TypeTag] =
    new TypeSymbol(tpe(mirror.classToType(tag[T].erasure)))

  implicit def anyWrapper[T: TypeTag](any: T) = new AnyWrapper(any)
  class AnyWrapper[T: TypeTag](source: T) {
    def reflection =
      vorm.reflection.reflection[T]
  }

}