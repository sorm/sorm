package vorm

package object mapping {

  import reflection._


  implicit class StringDdlNameSupport
    ( val string : String )
    {
      import com.google.common.base.CaseFormat._
      def ddlName
        : String
        = UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
    }

  implicit class ReflectionDdlColumnTypeSupport
    ( val reflection : Reflection )
    {
      def ddlColumnType
        ( isKeyPart : Boolean )
        = reflection match {
            case _ if reflection inheritsFrom reflectionOf[String]
              ⇒ if (isKeyPart) 
                  vorm.ddl.Column.Type.VarChar
                else 
                  vorm.ddl.Column.Type.Text
            case _ if reflection inheritsFrom reflectionOf[Boolean]
              ⇒ vorm.ddl.Column.Type.Boolean
            case _ if reflection inheritsFrom reflectionOf[Short]
              ⇒ vorm.ddl.Column.Type.SmallInt
            case _ if reflection inheritsFrom reflectionOf[Int]
              ⇒ vorm.ddl.Column.Type.Integer
            case _ if reflection inheritsFrom reflectionOf[Long]
              ⇒ vorm.ddl.Column.Type.BigInt
            case _ if reflection inheritsFrom reflectionOf[Float]
              ⇒ vorm.ddl.Column.Type.Float
            case _ if reflection inheritsFrom reflectionOf[Double]
              ⇒ vorm.ddl.Column.Type.Double
            case _ if reflection inheritsFrom reflectionOf[BigDecimal]
              ⇒ vorm.ddl.Column.Type.Decimal
          }
    }

}