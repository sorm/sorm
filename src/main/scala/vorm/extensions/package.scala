package vorm


package object extensions {

  implicit def anyWrapper[T: TypeTag](x: T) = new AnyWrapper(x)

}
