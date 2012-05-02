package vorm.reflection

import reflect.mirror

class TypeReflection(val mt: mirror.Type) extends Reflection {

  private def method(s: mirror.Symbol): MethodReflection = {
    type MethodType = {
      def params: List[mirror.Symbol]
      def resultType: mirror.Type
    }
    val t = s.typeSignature.asInstanceOf[MethodType]
    new MethodReflection(
      s.name.decoded.trim, this,
      t.params.map(p => reflection(p.typeSignature)),
      reflection(t.resultType)
    )
  }


  lazy val generics =
    mt.typeArguments.indices
      .map(i => new GenericReflection(i, this, reflection(mt.typeArguments(i))))

  lazy val properties =
    mt.members.filter(m => !m.isMethod && m.owner == mt.typeSymbol)
      .map(p => new PropertyReflection(p.name.decoded.trim, this, reflection(p.typeSignature)))

  lazy val methods =
    mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
      .map(method)

  lazy val constructors =
    mt.members.filter(m => m.kind == "constructor" && m.owner == mt.typeSymbol)
      .map(method)

  lazy val javaClass =
    mirror.typeToClass(mt)


  def is[T: TypeTag] =
    mt <:< tag[T].tpe


  def property(name: String) =
    properties.find(_.name == name).get

  def method(name: String): MethodReflection =
    methods.find(_.name == name).get


  override def toString = mt.toString
}
