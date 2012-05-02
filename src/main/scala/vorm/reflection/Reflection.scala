package vorm.reflection

import reflect.mirror

object Reflection {

  private val tpeCache = collection.mutable.Map[mirror.Type, Type]()
  def tpe(mt: mirror.Type): Type =
    try tpeCache(mt)
    catch {
      case _ => {
        val t = new Type(mt)
        tpeCache.update(mt, t)
        t
      }
    }


  class Type(mt: mirror.Type) {
    lazy val generics =
      mt.typeArguments.indices.map(i => new Generic(mt.typeArguments(i), this, i))

    lazy val properties =
      mt.members.filter(m => m.owner == mt && !m.isMethod)
        .map(p => new Property(p.name.decoded.trim, tpe(p.typeSignature), this))

    lazy val propertiesMap =
      properties.map(p => p.name -> p).toMap
  }

  class Generic(mt: mirror.Type, val owner: Type, val index: Int) extends Type(mt)

  class Property(val name: String, val tpe: Type, val owner: Type)

}

