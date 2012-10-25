package sorm.jdbc

import sext._
import org.slf4j.LoggerFactory
import org.joda.time.Period

trait JdbcConnectionLogging {
  private lazy val slf4j : org.slf4j.Logger = LoggerFactory getLogger this.getClass()
  private def logging = slf4j.isDebugEnabled
  private def log (s : Statement, time : Period) {
    slf4j.debug(
      "Executed statement in " + time + ":\n" +
      (("sql" -> s.sql) +: s.data.map(_.value).notEmpty.map("data" -> _) ++: Stream())
        .toMap.valueTreeString
    )
  }
  protected def log [ Z ] ( s : Statement ) ( f : => Z ) : Z = {
    if( logging ){
      Benchmarking.benchmarkDoing(log(s, _))(f)
    } else {
      f
    }
  }

}
