package sorm.mysql

import sorm.core.api.select._
import sorm.core.api.where._

class MysqlInstance 
  extends InstanceSelectSupport[ MysqlDriver ]
  {
    val driver = new MysqlDriver {}


  }

trait MysqlDriver extends DriverSelectSupport {

}
object MysqlDriver {

  implicit object IntEqualsSupport 
    extends EqualsSupport[ MysqlDriver, Int ]

  implicit object IntNotLargerSupport 
    extends NotLargerSupport[ MysqlDriver, Int ]

  implicit object StringEqualsSupport 
    extends EqualsSupport[ MysqlDriver, String ]

  implicit object StringRegexSupport 
    extends RegexSupport[ MysqlDriver, String ]

  implicit object TraversableExistsSupport 
    extends ExistsSupport[ MysqlDriver, Traversable[ _ ] ]

}
