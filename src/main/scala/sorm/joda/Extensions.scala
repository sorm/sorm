package sorm.joda

import org.joda.time._
import sext._, embrace._

object Extensions {
  
  implicit class DateTimeToJava ( val self : DateTime ) extends AnyVal {
    def toJava = new java.sql.Timestamp( self.getMillis )
  }

  implicit class LocalDateToJava ( val self : LocalDate ) extends AnyVal {
    def toJava = self.toDate.getTime $ (new java.sql.Date(_))
  }

  implicit class LocalTimeToJava ( val self : LocalTime ) extends AnyVal {
    def toJava = new java.sql.Time( self.getMillisOfDay )
  }



  implicit class DateToJoda ( val self : java.sql.Date ) extends AnyVal {
    def toJoda = LocalDate.fromDateFields(self)
  }

  implicit class TimeToJoda ( val self : java.sql.Time ) extends AnyVal {
    def toJoda = LocalTime.fromDateFields(self)
  }

  implicit class TimestampToJoda ( val self : java.sql.Timestamp ) extends AnyVal {
    def toJoda = new DateTime(self.getTime)
  }



}