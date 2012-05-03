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

  //case class PropertyDescriptor(
  //  name: String,
  //  t: Type
  //)
  //case class ArgumentDescriptor(
  //  name: String,
  //  t: Type
  //)
  //case class MethodDescriptor(
  //  name: String,
  //  arguments: List[ArgumentDescriptor],
  //  result: Type
  //)

  //  lazy val methodParameters =
  //    mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
  //      .map(s =>
  //        s.name.decoded.trim ->
  //          s.asInstanceOf[{def params: List[mirror.Symbol]}].params
  //            .map(s => reflection(s.typeSignature))
  //      )
  //      .toMap

  //  lazy val methods = {
  //    def method(s: mirror.Symbol) = {
  //      type MethodType = {
  //        def params: List[mirror.Symbol]
  //        def resultType: mirror.Type
  //      }
  //      val t = s.typeSignature.asInstanceOf[MethodType]
  //      new MethodDescriptor(
  //        s.name.decoded.trim,
  //        t.params.map(p => ArgumentDescriptor(s.name.decoded, reflection(p.typeSignature))),
  //        reflection(t.resultType)
  //      )
  //    }
  //    mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
  //      .map(method)
  //  }

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


  //  lazy val properties =
  //    mt.members.filter(m => !m.isMethod && m.owner == mt.typeSymbol)
  //      .map(p => new PropertySymbol(p.name.decoded.trim, this, reflection(p.typeSignature)))
  //
  //  lazy val methods =
  //    mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
  //      .map(method)
  //
  //  lazy val constructors =
  //    mt.members.filter(m => m.kind == "constructor" && m.owner == mt.typeSymbol)
  //      .map(method)
  //
  //
  //
  //  def is[T: TypeTag] =
  //    mt <:< tag[T].tpe
  //
  //
  //  def property(name: String) =
  //    properties.find(_.name == name).get
  //
  //  def method(name: String): MethodSymbol =
  //    methods.find(_.name == name).get


  override def toString = mt.toString
}
