package vorm.reflection

class TypeSymbol(val t: Type) extends Symbol {

  lazy val generics =
    t.generics.indices.map(i => new GenericSymbol(this, i, t.generics(i)))

  //  def properties = propertyByNameMap.values
  //  def property(name: String) = propertyByNameMap(name)
  lazy val properties =
    t.properties
      .map {case (name, t) => new PropertySymbol(this, name, t)}
      .map(ps => ps.name -> ps)
      .toMap

  lazy val methods =
    t.methods
      .map {case (name, (argTypes, resultType)) =>
        name ->
          new MethodSymbol(this, name, argTypes, resultType)
      }
      .toMap

  override def toString = t.toString
}
