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
object MysqlDriver extends DriverSpecificOperationsSupport[MysqlDriver] {

  implicit object IntEqualsSupport 
    extends DriverEqualsSupport[ Int ]

  implicit object IntNotLargerSupport 
    extends DriverNotLargerSupport[ Int ]

  implicit object StringEqualsSupport 
    extends DriverEqualsSupport[ String ]

  implicit object StringRegexSupport 
    extends DriverRegexSupport[ String ]

  implicit object TraversableExistsSupport 
    extends DriverExistsSupport[ Traversable[ _ ] ]

}
