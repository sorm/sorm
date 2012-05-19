package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{Connection, Statement => JStatement}

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAPI(x: Connection) = new ConnectionAPI(x)

}