package vorm.reflection

import reflect.mirror

class Type(mt: mirror.Type) {


  lazy val javaClass  =
    mirror.typeToClass(mt)
  lazy val name       =
    mt.typeSymbol.name.decoded
  lazy val generics   =
    mt.typeArguments
      .map(tpe(_))
  lazy val properties =
    mt.members.filter(m => !m.isMethod && m.owner == mt.typeSymbol)
      .map(s => s.name.decoded.trim -> tpe(s.typeSignature))
      .toMap

  lazy val methods = {
    def method(s: mirror.Symbol) = {
      type MethodType = {
        def params: List[mirror.Symbol]
        def resultType: mirror.Type
      }
      val t = s.typeSignature.asInstanceOf[MethodType]
      val name = s.name.decoded.trim
      val parameters =
        t.params.map(p => (s.name.decoded, tpe(p.typeSignature))).toMap
      val resultType = tpe(t.resultType)
      name -> (parameters -> resultType)
    }
    mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
      .map(method).toMap
  }


  def methodResult(name: String, instance: AnyRef, args: List[Any] = Nil) =
    mirror.invoke(
      instance,
      mt.member(mirror.newTermName(name).asInstanceOf[mirror.Name])
    )(args: _*)

  val propertyValue = methodResult(_: String, _: AnyRef)


  def inherits(t: Type): Boolean =
  //    mt <:< tag[T].tpe
    throw new NotImplementedError


  //  lazy val constructors =
  //    mt.members.filter(m => m.kind == "constructor" && m.owner == mt.typeSymbol)
  //      .map(method)
  //

  override def toString = mt.toString
}
