package sorm.core

import org.joda.time.DateTime

trait CurrentDateTime {
  protected def driver : Driver

  private lazy val deviation
    = System.currentTimeMillis - driver.now().getMillis

  /**
   * Current time at DB server in milliseconds. Effectively fetches the date only once to calculate the deviation.
   */
  def nowMillis() = System.currentTimeMillis - deviation
  /**
   * Current DateTime at DB server. Effectively fetches the date only once to calculate the deviation.
   */
  def now() = new DateTime(nowMillis())

}
