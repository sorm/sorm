package vorm

import reflect.mirror

package object reflection {

  private val tpeCache =
    collection.mutable.Map[mirror.Type, Type]()

  def tpe[T](mt: mirror.Type): Type =
    try tpeCache(mt)
    catch {
      case _ => {
        val t = new Type(mt)
        tpeCache.update(mt, t)
        t
      }
    }

  //  def tpe[T: TypeTag]: Type =
  //    tpe(mirror.classToType(tag[T].erasure))
  //
  //  def tpe[T: TypeTag](instance: T): Type =
  //    tpe[T]

  //  private val typeSymbolCache =
  //    collection.mutable.Map[mirror.Type, TypeSymbol]()
  //
  //  def typeSymbol[T](mt: mirror.Type) =
  //    try typeSymbolCache(mt)
  //    catch {
  //      case _ => {
  //        val s = new TypeSymbol(tpe(mirror.classToType(tag[T].erasure)))
  //        typeSymbolCache.update(mt, s)
  //        s
  //      }
  //    }
  //

  def typeSymbol[T: TypeTag] =
    new TypeSymbol(tpe(mirror.classToType(tag[T].erasure)))

  //  def is[T2, T: TypeTag](instance: T): Boolean =
  //    tag[T].tpe <:< tag[T2].tpe

  implicit def anyWrapper[T: TypeTag](any: T) = new AnyWrapper(any)
  class AnyWrapper[T: TypeTag](source: T) {
    def is[T2: TypeTag] =
      tag[T].tpe <:< tag[T2].tpe

    def typeSymbol =
      vorm.reflection.typeSymbol[T]

    //    def tpe =
    //      vorm.reflection.tpe(source)
  }

}