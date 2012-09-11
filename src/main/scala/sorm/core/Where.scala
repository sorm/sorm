package sorm.core

object Where {

  sealed trait Where
  case class Equal ( p : String, v : Any ) extends Where
  case class NotEqual ( p : String, v : Any ) extends Where
  case class Or ( l : Where, r : Where ) extends Where
  case class And ( l : Where, r : Where ) extends Where

  trait Dsl {
    
    implicit class StringWhere ( val p : String ) extends AnyVal {
      def equal ( v : Any ) = Equal(p, v)
      def notEqual ( v : Any ) = NotEqual(p, v)
    }

    implicit class WhereWhere ( val a : Where ) extends AnyVal {
      def or ( b : Where ) = Or(a, b)
      def and ( b : Where ) = And(a, b)
    }

  }

}
