package sorm

object Dsl {

  import Querier._

  implicit class DslFilter(val a: Filter) extends AnyVal {
    def or(b: Filter) = Or(a, b)
    def and(b: Filter) = And(a, b)
  }

  implicit class DslString(val p: String) extends AnyVal {
    def equal(v: Any)          = Equal(p, v)
    def notEqual(v: Any)       = NotEqual(p, v)
    def smaller(v: Any)        = Smaller(p, v)
    def smallerOrEqual(v: Any) = SmallerOrEqual(p, v)
    def larger(v: Any)         = Larger(p, v)
    def largerOrEqual(v: Any)  = LargerOrEqual(p, v)
    def like(v: Any)           = Like(p, v)           
    def notLike(v: Any)        = NotLike(p, v)        
    def regex(v: Any)          = Regex(p, v)          
    def notRegex(v: Any)       = NotRegex(p, v)       
    def in(v: Any)             = In(p, v)             
    def notIn(v: Any)          = NotIn(p, v)          
    def contains(v: Any)       = Contains(p, v)       
    def notContains(v: Any)    = NotContains(p, v)    
    def constitutes(v: Any)    = Constitutes(p, v)    
    def notConstitutes(v: Any) = NotConstitutes(p, v) 
    def includes(v: Any)       = Includes(p, v)       
    def notIncludes(v: Any)    = NotIncludes(p, v)    
  }

}
