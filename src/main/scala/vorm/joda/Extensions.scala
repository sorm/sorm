package vorm.joda

import org.joda.time._

import vorm._
import extensions._

object Extensions {
  
  implicit class DateTimeAdapter
    ( self : DateTime )
    {
      def toJava = new java.sql.Timestamp( self.getMillis )
    }

  implicit class LocalDateAdapter
    ( self : LocalDate )
    {
      def toJava = new java.sql.Date( self.toDateMidnight.getMillis )
    }

  implicit class LocalTimeAdapter
    ( self : LocalTime )
    {
      def toJava = new java.sql.Time( self.getMillisOfDay )
    }



  implicit class DateAdapter
    ( self : java.sql.Date )
    {
      def toJoda = LocalDate.fromDateFields(self)
    }

  implicit class TimeAdapter
    ( self : java.sql.Time )
    {
      def toJoda = LocalTime.fromDateFields(self)
    }

  implicit class TimestampAdapter
    ( self : java.sql.Timestamp )
    {
      def toJoda = new DateTime(self.getTime)
    }



}