package sorm.test.general

import org.scalatest.{SequentialNestedSuiteExecution, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import core.DbType
import sext._, embrace._
import org.joda.time._
import sorm.test.TestingInstances

@RunWith(classOf[JUnitRunner])
class FieldTestSuite1 extends FunSuite with ShouldMatchers with SequentialNestedSuiteExecution {
  import FieldTestSuite1._
  def entities
    = Set() +
      Entity[Tag](unique = Set() + Seq("name")) +
      Entity[Category](unique = Set() + Seq("name")) +
      Entity[Label](unique = Set() + Seq("name")) +
      Entity[Format](unique = Set() + Seq("name")) +
      Entity[Task](indexed = Set() + Seq("sourceId") + Seq("opened") + Seq("closed") + Seq("started") + Seq("outdated") + Seq("closed", "outdated", "started")) +
      Entity[Artist](unique = Set() + Seq("sourceId")) +
      Entity[Mp3](unique = Set() + Seq("sourceId")) +
      Entity[Copyright](indexed = Set() + Seq("year")) +
      Entity[Genre](unique = Set() + Seq("name")) +
      Entity[Album](unique = Set() + Seq("sourceId"))
  def instance( t: DbType ) = TestingInstances.instance(entities, t)

  test("PostgreSQL initialization") { instance(DbType.Postgres).close() }
  test("HSQLDB initialization") { instance(DbType.Hsqldb).close() }
  test("H2 initialization") { instance(DbType.H2).close() }
  test("MySQL initialization") { instance(DbType.Mysql).close() }
  //test("Oracle initialization") { instance(DbType.Oracle).close() }

}
object FieldTestSuite1 {

  case class Album
    ( sourceId : String,
      name : String,
      format : Format,
      artists : Seq[Artist],
      artistNames : Seq[String],
      mp3s : Seq[Mp3],
      tracklist : Seq[String],
      genres : Set[Genre],
      categories : Set[Category],
      tagScores : Map[Tag, Int],
      relatedItemsAsins : Seq[String],
      relatedArtists : Seq[Artist],
      rating : (Int, Int, Int, Int, Int),
      releaseDate : Option[LocalDate],
      originalReleaseDate : Option[LocalDate],
      copyright : Option[Copyright],
      label : Option[Label],
      duration : Option[Int] )

  case class Label
    ( name : String )

  case class Copyright
    ( text : String,
      year : Short )

  case class Mp3
    ( sourceId : String,
      name : String,
      artists : Seq[Artist],
      duration : Option[Int] )

  case class Genre
    ( name : String )

  case class Tag
    ( name : String )

  case class Category
    ( name : String )

  case class Format
    ( name : String )

  case class Artist
    ( sourceId : String,
      names : Seq[String] )

  case class Task
    ( sourceId : String,
      opened : DateTime,
      started : Option[DateTime] = None,
      closed : Option[DateTime] = None,
      failure : Option[String] = None,
      outdated : Boolean = false,
      generated : Boolean = false )
}