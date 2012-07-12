package vorm


package object extensions {

  implicit def anyExtensions[T: TypeTag](x: T) = new AnyExtensions(x)

  implicit def mapExtensions[K, V](x: Map[K, V]) = new MapExtensions[K, V](x)

  implicit class TupleFoldableExtender
    [ ItemT, ResultT ]
    ( tuple : (ResultT, Traversable[ItemT]) )
    {
      private val (initial, foldable) = tuple
      def foldRight
        ( f : (ItemT, ResultT) => ResultT )
        = (foldable foldRight initial)(f)
      def foldLeft
        ( f : (ResultT, ItemT) => ResultT )
        = (foldable foldLeft initial)(f)
    }

}
