package vorm.reflection

/**
 * Seems like a bit too much
 */
private[reflection] class AnyRefExtensions[T <: AnyRef : TypeTag](x: T) {

  lazy val t = tpe[T]

  def propertyValueByNameMap =
    t.properties
      .map(p => p.name -> t.propertyValue(p.name, x))
      .toMap

  def propertyValue(name: String) =
    t.propertyValue(name, x)



}
