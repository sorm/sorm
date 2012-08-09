package vorm


package object extensions {

  implicit def mapExtensions[K, V](x: Map[K, V]) = new MapExtensions[K, V](x)

  implicit class TraversableExtensions
    [ ItemT,
      TraversableT[ItemT] <: Traversable[ItemT] ]
    ( traversable : TraversableT[ItemT] )
    {
      import collection.generic.CanBuildFrom

      def zipBy
        [ ResultItemT,
          ResultT ]
        ( f : ItemT ⇒ ResultItemT )
        ( implicit bf : CanBuildFrom[TraversableT[ItemT], (ItemT, ResultItemT), ResultT] )
        : ResultT
        = traversable.map(x ⇒ x → f(x)).asInstanceOf[ResultT]
    }

  implicit class AnyExtensions[A](x: A) {
    def tap[ResultT](f: A => ResultT) = {
      f(x)
      x
    }

    def as[ResultT](f: A => ResultT) =
      f(x)

    def applyTo[ResultT](f: A => ResultT) =
      f(x)


    def isEmpty = {
      x match {
        case null | () => true
        case x: Boolean => !x
        case x: Byte => x == 0.toByte
        case x: Short => x == 0.toShort
        case x: Char => x == 0.toChar
        case x: Int => x == 0
        case x: Long => x == 0l
        case x: Float => x == 0f
        case x: Double => x == 0d
        case x: Product => x.productArity == 0
        case _ => false
      }
    }

    def notEmpty =
      if (x.isEmpty) None else Some(x)

    def satisfying(p: A => Boolean): Option[A] =
      if (p(x)) Some(x) else None

    def trace() {
      Console.println(x)
    }

    def prettyString
      : String
      = x match {
          case x : Traversable[_] =>
            x.getClass.getSimpleName + ":\n" +
            x.view
              .map{ _.prettyString }
              .mkString(",\n")
              .indent(2)
          case x : Tuple2[_, _] =>
            ( x._1.prettyString + " ->\n" +
              x._2.prettyString )
          case x : Product =>
            x.getClass.getName + ":\n" +
            x.productIterator
              .map{ _.prettyString }
              .mkString(",\n")
              .indent(2)
          case x => 
            x.toString
        }

    def trying
      [ ResultT ]
      ( f : A => ResultT )
      = try Some(f(x)) catch { case _ => None }

    def unfold
      [ B ]
      ( f : A => Option[(B, A)] )
      : Stream[B]
      = f(x) match {
          case None => Stream.empty
          case Some((b, a)) => b #:: a.unfold(f)
        }

    def foldTo
      [ B ]
      ( xs : Traversable[B] )
      ( f : (B, A) ⇒ A)
      = xs.foldRight(x)(f)

    def foldFrom
      [ B ]
      ( xs : Traversable[B] )
      ( f : (A, B) ⇒ A)
      = xs.foldLeft(x)(f)

    def some
      = Some(x)


  }

  implicit class OptionExtensions
    [ T ]
    ( option : Option[_])
    {
      def +: ( x : T ) = Option(x)
    }

  implicit class StringExtensions
    ( s : String )
    {
      def notEmpty
        = if( s.isEmpty ) None else Some(s)
      def indent
        ( i : Int )
        = prependLines(" " * i) 
      def prependLines
        ( p : String )
        = s.lines
            .reduceOption{ _ + "\n" + p + _ }
            .map{ p + _ }
            .getOrElse( p )
    }

}
