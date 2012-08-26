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
            case _ if reflection inheritsFrom Reflection[String]
              ⇒ if (isKeyPart) 
                  sorm.ddl.Column.Type.VarChar
                else 
                  sorm.ddl.Column.Type.Text
            case _ if reflection inheritsFrom Reflection[Boolean]
              ⇒ sorm.ddl.Column.Type.Boolean
            case _ if reflection inheritsFrom Reflection[Short]
              ⇒ sorm.ddl.Column.Type.SmallInt
            case _ if reflection inheritsFrom Reflection[Int]
              ⇒ sorm.ddl.Column.Type.Integer
            case _ if reflection inheritsFrom Reflection[Long]
              ⇒ sorm.ddl.Column.Type.BigInt
            case _ if reflection inheritsFrom Reflection[Float]
              ⇒ sorm.ddl.Column.Type.Float
            case _ if reflection inheritsFrom Reflection[Double]
              ⇒ sorm.ddl.Column.Type.Double
            case _ if reflection inheritsFrom Reflection[BigDecimal]
              ⇒ sorm.ddl.Column.Type.Decimal
          }
    }


  type SettingsMap = collection.Map[Reflection, EntitySettings]


}