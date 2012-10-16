package sorm

package object structure {

  import reflection._
  import com.google.common.base.CaseFormat._

  def ddlName ( string : String )
    : String
    = UPPER_CAMEL.to( LOWER_UNDERSCORE, string )


  implicit class ReflectionDdlColumnTypeSupport
    ( reflection : Reflection )
    {
      def ddlColumnType
        ( isKeyPart : Boolean )
        = reflection match {
            case _ if reflection <:< Reflection[String]
              ⇒ if (isKeyPart) 
                  sorm.ddl.Column.Type.VarChar
                else 
                  sorm.ddl.Column.Type.Text
            case _ if reflection <:< Reflection[Boolean]
              ⇒ sorm.ddl.Column.Type.Boolean
            case _ if reflection <:< Reflection[Short]
              ⇒ sorm.ddl.Column.Type.SmallInt
            case _ if reflection <:< Reflection[Int]
              ⇒ sorm.ddl.Column.Type.Integer
            case _ if reflection <:< Reflection[Long]
              ⇒ sorm.ddl.Column.Type.BigInt
            case _ if reflection <:< Reflection[Float]
              ⇒ sorm.ddl.Column.Type.Float
            case _ if reflection <:< Reflection[Double]
              ⇒ sorm.ddl.Column.Type.Double
            case _ if reflection <:< Reflection[BigDecimal]
              ⇒ sorm.ddl.Column.Type.Decimal
          }
    }


  type SettingsMap = collection.Map[Reflection, EntitySettings]


}