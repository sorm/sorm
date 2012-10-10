package sorm.core

import org.joda.time.DateTime
import sorm.jdbc._

trait CurrentDateTime {
  protected def driver : Driver

  private lazy val deviation
    = System.currentTimeMillis - driver.now().getMillis

  def millis = System.currentTimeMillis - deviation
  /**
   * Current DateTime at DB server. Effectively fetches the date only once to
   * calculate the deviation.
   */
  def dateTime = new DateTime(millis)

}
