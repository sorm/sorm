package vorm

import extensions._

package object ddl {
  def quote ( x : String ) = "`" + x + "`"
}