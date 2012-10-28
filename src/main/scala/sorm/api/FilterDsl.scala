package sorm.api

object FilterDsl {

  import Filter._

  implicit class WhereWhere(val a: Filter) extends AnyVal {
    def or(b: Filter) = Or(a, b)
    def and(b: Filter) = And(a, b)
  }

  implicit class StringWhere(val p: String) extends AnyVal {
    def equal(v: Any)          = Equal(p, v)
    def notEqual(v: Any)       = NotEqual(p, v)
    def smaller(v: Any)        = Smaller(p, v)
    def smallerOrEqual(v: Any) = SmallerOrEqual(p, v)
    def larger(v: Any)         = Larger(p, v)
    def largerOrEqual(v: Any)  = LargerOrEqual(p, v)
    //  todo: add the rest
  }

}
