package vorm

package object mapping {


  implicit class StringSqlifiedSupport 
    ( val x : String )
    extends AnyVal
    {
      def sqlified
        : String
        = throw new NotImplementedError
    }


}