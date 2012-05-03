package vorm

package object extensions {

  implicit def anyWrapper[T: TypeTag](x: T) = new AnyWrapper(x)
  class AnyWrapper[T: TypeTag](x: T) {
    def as[Result](closure: T => Result) =
      closure(x)

    def isInstanceOf1[T2: TypeTag] =
      tag[T].tpe <:< tag[T2].tpe

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

    def notEmptyValue =
      if (isEmpty) None else Some(x)

    def matchingValue(p: T => Boolean) =
      if (p(x)) Some(x) else None

    def println() {
      Console.println(x)
    }
  }
}
