package vorm

import reflect.mirror

package object reflection {

  private val refectionCache = collection.mutable.Map[mirror.Type, TypeReflection]()

  def reflection[T](mt: mirror.Type): TypeReflection =
    try refectionCache(mt)
    catch {
      case _ => {
        val t = new TypeReflection(mt)
        refectionCache.update(mt, t)
        t
      }
    }

  def reflection[T: TypeTag]: TypeReflection =
    reflection(mirror.classToType(tag[T].erasure))

  def reflection[T: TypeTag](instance: T): TypeReflection =
    reflection[T]

}