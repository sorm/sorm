package vorm

package object structure {

  import reflection._


  implicit class StringDdlNameSupport
    ( string : String )
    {
      import com.google.common.base.CaseFormat._
      def asDdlName
        : String
        = UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
    }


  implicit class ReflectionDdlColumnTypeSupport
    ( reflection : Reflection )
    {
      def ddlColumnType
        ( isKeyPart : Boolean )
        = reflection match {
            case _ if reflection inheritsFrom Reflection[String]
              ⇒ if (isKeyPart) 
                  vorm.ddl.Column.Type.VarChar
                else 
                  vorm.ddl.Column.Type.Text
            case _ if reflection inheritsFrom Reflection[Boolean]
              ⇒ vorm.ddl.Column.Type.Boolean
            case _ if reflection inheritsFrom Reflection[Short]
              ⇒ vorm.ddl.Column.Type.SmallInt
            case _ if reflection inheritsFrom Reflection[Int]
              ⇒ vorm.ddl.Column.Type.Integer
            case _ if reflection inheritsFrom Reflection[Long]
              ⇒ vorm.ddl.Column.Type.BigInt
            case _ if reflection inheritsFrom Reflection[Float]
              ⇒ vorm.ddl.Column.Type.Float
            case _ if reflection inheritsFrom Reflection[Double]
              ⇒ vorm.ddl.Column.Type.Double
            case _ if reflection inheritsFrom Reflection[BigDecimal]
              ⇒ vorm.ddl.Column.Type.Decimal
          }
    }


  type Settings = collection.Map[Reflection, EntitySettings]


}