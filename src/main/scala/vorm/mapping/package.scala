package vorm

package object mapping {


  implicit class StringDdlFormattedSupport
    ( val string : String )
    {
      import com.google.common.base.CaseFormat._
      def ddlFormatted
        : String
        = UPPER_CAMEL.to(LOWER_UNDERSCORE, string)
    }


}