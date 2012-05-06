package vorm.reflection

private[reflection] class AnyExtensions[T: TypeTag](x: T) {

  def tpe = vorm.reflection.tpe[T]
}
