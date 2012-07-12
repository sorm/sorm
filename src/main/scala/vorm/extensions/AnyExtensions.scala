package vorm.extensions

class AnyExtensions[T: TypeTag](x: T) {
  def tap[ResultT](f: T => ResultT) = {
    f(x)
    x
  }

  def as[ResultT](f: T => ResultT) =
    f(x)

  /**
   * Generics-aware version
   */
  def isInstanceOf1[ComparedT: TypeTag] =
    tag[T].tpe <:< tag[ComparedT].tpe

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
      case _ => false
    }
  }

  def asNonEmpty =
    if (isEmpty) None else Some(x)

  def asSatisfying(p: T => Boolean): Option[T] =
    if (p(x)) Some(x) else None

  def println() {
    Console.println(x)
  }

  def trying
    [ ResultT ]
    ( f : T => ResultT )
    = try Some(f(x)) catch { case _ => None }
}