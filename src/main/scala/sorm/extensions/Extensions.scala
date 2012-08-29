package sorm.extensions

import util.Try
import reflect.runtime.universe._
import reflect.runtime.currentMirror

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
          = s.lines.toStream match {
              case h +: t =>
                ( ("- " + h) +: 
                  t.map{"|  " + _}
                ) .mkString("\n")
              case _ => ""
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
          case null =>
            "null"
          case x =>
            x.toString
        }
      }

    def trying
      [ ResultT ]
      ( f : A => ResultT )
      = Try(f(x)).toOption

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
  implicit class AnyInstanceOf[ A : TypeTag ]( x : A ) {
    def toInstanceOf[ T : TypeTag ] : Option[T]
      = {
        def test
          = currentMirror.runtimeClass(typeOf[T]) match {
              case c if c.isPrimitive => typeOf[A] <:< typeOf[T]
              case c => c.isAssignableFrom(x.getClass) || typeOf[A] <:< typeOf[T]
            }
        if( test ) Some( x.asInstanceOf[T] )
        else None
      }
  }
  implicit class AnyFunctional[ A ]( α : A ) {

    def unfold
      [ B ]
      ( ƒ : A => Option[(B, A)] )
      : Stream[B]
      = ƒ(α) map {case (β, α) ⇒ β #:: (α unfold ƒ)} getOrElse Stream()    

    def unfold1
      ( ƒ : A => Option[A] )
      : Stream[A]
      = ƒ(α) map (α ⇒ α #:: (α unfold1 ƒ)) getOrElse Stream()

    def iterate
      ( ƒ : A => A )
      : Stream[A]
      = α #:: (ƒ(α) iterate ƒ)

    def foldTo
      [ B ]
      ( σ : Traversable[B] )
      ( ƒ : (B, A) ⇒ A)
      = (σ foldRight α)(ƒ)

    def foldFrom
      [ B ]
      ( σ : Traversable[B] )
      ( ƒ : (A, B) ⇒ A)
      = (σ foldLeft α)(ƒ)

  }

}
