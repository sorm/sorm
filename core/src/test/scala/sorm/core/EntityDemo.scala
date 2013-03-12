package sorm.core

object EntityDemo extends App {

  // Having this class
  case class A( a : Int, b : String )

  // triggering the macro conversion with the following
  // entity[ A ]( Set(), Set() )
  // should result in
  val e = new Entity[ A ]{
    val indexed : Set[ Key[ A ] ] = Set()
    val unique : Set[ Key[ A ] ] = Set()
    def toPersisted( value : A, idValue : Long )
      = new A( value.a, value.b ) with Persisted {
          val id = idValue
          // a copy of `A` with `Persisted` mixed in
          override def copy
            ( a : Int = a, 
              b : String = b ) 
            : A with Persisted 
            = toPersisted( A(a, b), id )
          // product elements of `A` prepended with `id`
          override def productElement( n : Int ) : Any = n match {
            case 0 => id
            case 1 => a
            case 2 => b
            case _ => throw new IndexOutOfBoundsException( n.toString ) 
          }
          // product arity of `A` plus 1 (implying `id`)
          override def productArity = 3
          override def equals( that : Any ) = that match {
            case that : Persisted => id == that.id && super.equals( that )
            case _ => false
          }
        }
  }


  val value = A( 2, "ABC" )

  assert(e.toPersisted(value, 1).id == 1)
  assert(e.toPersisted(value, 2).copy(50).id == 2)
  assert(e.toPersisted(value, 3).copy(50).a == 50)

}