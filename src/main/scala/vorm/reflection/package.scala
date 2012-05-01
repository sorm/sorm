package vorm

package object reflection {

  private val tpeCache = collection.mutable.Map[TypeTag[_], Type]()
  def tpe[T](implicit tag: TypeTag[T]) =
    try tpeCache(tag)
    catch {
      case _ => {
        val t = new Type(tag)
        tpeCache.update(tag, t)
        t
      }
    }

}

