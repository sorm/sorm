package sorm.jdbc

import sext._
import org.slf4j.LoggerFactory
import org.joda.time.Period

trait JdbcConnectionLogging {
  private lazy val logger : org.slf4j.Logger = LoggerFactory getLogger this.getClass()
  private def logging = logger.isDebugEnabled

  protected def logStatement (s : Statement) {
    logger.debug(
      "Executing statement:\n" +
      (("sql" -> s.sql) +: s.data.map(_.value).notEmpty.map("data" -> _) ++: Stream())
        .toMap.valueTreeString
    )
  }

  protected def executeLoggingBenchmark [ Z ] ( f : => Z ) : Z = {
    if( logging ){
      Benchmarking.benchmarkDoing(ns => logger.debug("Executed statement in %,.3fms:\n".format(ns / 1000000d)))(f)
    } else {
      f
    }
  }

}
