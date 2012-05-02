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
    tpe(mirror.classToType(tag[T].erasure))

  def tpe[T: TypeTag](instance: T): Type =
    tpe[T]


  class Type(val mt: mirror.Type) {

    private def method(s: mirror.Symbol): Method = {
      type MethodType = {
        def params: List[mirror.Symbol]
        def resultType: mirror.Type
      }
      val t = s.typeSignature.asInstanceOf[MethodType]
      new Method(
        s.name.decoded.trim, this,
        t.params.map(p => tpe(p.typeSignature)),
        tpe(t.resultType)
      )
    }


    lazy val generics =
      mt.typeArguments.indices
        .map(i => new Generic(i, this, tpe(mt.typeArguments(i))))

    lazy val properties =
      mt.members.filter(m => !m.isMethod && m.owner == mt.typeSymbol)
        .map(p => new Property(p.name.decoded.trim, this, tpe(p.typeSignature)))

    lazy val methods =
      mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
        .map(method)

    lazy val constructors =
      mt.members.filter(m => m.kind == "constructor" && m.owner == mt.typeSymbol)
        .map(method)

    lazy val javaClass =
      mirror.typeToClass(mt)


    def property(name: String) =
      properties.find(_.name == name).get

    def method(name: String): Method =
      methods.find(_.name == name).get


    override def toString = mt.toString
  }


  class Generic(val index: Int, val owner: Type, val tpe: Type) {
    override def toString = tpe.toString + "@" + owner.toString
  }

  class Property(val name: String, val owner: Type, val tpe: Type) {
    def value(instance: AnyRef) =
      mirror.invoke(instance, owner.mt.member(mirror.newTermName(name).asInstanceOf[mirror.Name]))()

    override def toString = owner.toString + "." + name + ": " + tpe.toString
  }

  class Method(val name: String, val owner: Type, val argumentTypes: List[Type], val resultType: Type) {

    def invoke(instance: AnyRef, args: List[Any] = Nil) =
      mirror.invoke(
        instance,
        owner.mt.member(
          mirror.newTermName(name).asInstanceOf[mirror.Name]
        )
      )(args: _*)

    override def toString = owner.toString + "." + name + "(" + argumentTypes.mkString(", ") + "): " + resultType.toString
  }

}

