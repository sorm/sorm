package vorm


package object extensions {

  implicit def anyExtensions[T: TypeTag](x: T) = new AnyExtensions(x)

  implicit def mapExtensions[K, V](x: Map[K, V]) = new MapExtensions[K, V](x)
}
