package sorm.extensions


object Extensions {

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

    def satisfying1
      ( p : A => Boolean )
      : Either[A, A]
      = if( p(x) ) Left(x)
        else Right(x)

    def trace()
      = {
        Console.println(x)
        x
      }

    def prettyString
      : String
      = {
        def indent ( s : String )
          = {
            val lines = s.lines.toStream
            ( lines.headOption.map{"-  " + _} ++:
              lines.tail.map{"|  " + _} )
              .mkString("\n")
          }
        x match {
          case x : Traversable[_] =>
            x.stringPrefix + ":\n" +
            x.view
              .map{ _.prettyString }
              .map{ indent }
              .mkString("\n")
          case x : Product if x.productArity == 0 =>
            x.productPrefix
          case x : Product =>
            x.productPrefix + ":\n" +
            x.productIterator
              .map{ _.prettyString }
              .map{ indent }
              .mkString("\n")
          case x =>
            x.toString
        }
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
      def splitBy
        ( splitter : String )
        : (String, String)
        = s.indexOf(splitter) match {
            case -1 => (s, "")
            case i =>
              val (a, b) = s.splitAt(i)
              (a, b.drop(splitter.size))
          }
    }

}
