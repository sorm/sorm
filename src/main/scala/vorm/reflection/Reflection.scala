package vorm.reflection

import reflect.mirror

object Reflection {

  private val tpeCache = collection.mutable.Map[mirror.Type, Type]()
  def tpe[T](mt: mirror.Type): Type =
    try tpeCache(mt)
    catch {
      case _ => {
        val t = new Type(mt)
        tpeCache.update(mt, t)
        t
      }
    }
  def tpe[T: TypeTag]: Type =
    tpe(tag[T].tpe)
  def tpe[T: TypeTag](instance: T): Type =
    tpe(tag[T].tpe)

  class Type(mt: mirror.Type) {
    lazy val generics =
      mt.typeArguments.indices
        .map(i => new Generic(mt.typeArguments(i), this, i))

    lazy val properties =
      mt.members.filter(m => !m.isMethod)
        .map(p => new Property(p.name.decoded.trim, tpe(p.typeSignature), this))

    lazy val propertyByNameMap =
      properties.map(p => p.name -> p).toMap

    override def toString = mt.toString
  }

  class Generic(mt: mirror.Type, val owner: Type, val index: Int) extends Type(mt) {
    override def toString = super.toString + "@" + owner.toString
  }

  class Property(val name: String, val tpe: Type, val owner: Type) {
    override def toString = owner.toString + "." + name + ": " + tpe.toString
  }

}

