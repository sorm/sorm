package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Sorm._
import samples._
import org.joda.time.{LocalDate, DateTime}

@RunWith(classOf[JUnitRunner])
class SophisticatedDomainSuite extends FunSuite with ShouldMatchers {
  import SophisticatedDomainSuite._

  test("Date not storing None in Mysql bugfix"){
    val db = TestingInstance.mysql(
      Entity[Task]( unique = Set(Seq("opened"), Seq("closed")) )
    )
    db.save(Task(PageType.Album, "", db.now()))
    db.fetchById[Task](1l).closed should equal(None)
  }
  test("Unique keys support"){
    val db = TestingInstance.h2(
      Entity[Settings](),
      Entity[Task]( unique = Set(Seq("opened"), Seq("closed")) ),
      Entity[Album](),
      Entity[Track](),
      Entity[Genre]( unique = Set(Seq("name")) ),
      Entity[Artist]()
    )
    db.save(Genre("Rock"))
  }
  test("Entities Set in instance parameters cause unregistered entity exception bugfix"){
    val db =
      new Instance(
        Set(
          Entity[Settings](),
          Entity[Task]( indexed = Set(Seq("opened"), Seq("closed")) ),
          Entity[Album](),
          Entity[Track](),
          Entity[Genre]( unique = Set(Seq("name")) ),
          Entity[Artist]()
        ),
        "jdbc:h2:mem:test",
        initMode = InitMode.DropAllCreate
      )
    db.save(Genre("Rock"))
  }
  test("Correct instantiation doesn't throw exceptions"){
    new Instance(
      Entity[Settings]() +:
      Entity[Task]( indexed = Set(Seq("opened"), Seq("closed")) ) +:
      Entity[Album]() +:
      Entity[Track]() +:
      Entity[Genre]() +:
      Entity[Artist]() +:
      Nil,
      "jdbc:h2:mem:test",
      initMode = InitMode.DropAllCreate
    )
  }
  test("Saving goes fine"){
    val db = TestingInstance.h2(
      Entity[Settings](),
      Entity[Task](),
      Entity[Album](),
      Entity[Track](),
      Entity[Genre](),
      Entity[Artist]()
    )
    val rock = db.save(Genre("Rock"))
    val hardRock = db.save(Genre("Hard Rock"))
    val pop = db.save(Genre("Pop"))
  }

}
object SophisticatedDomainSuite {
  case class Settings
    ( listingUrlTemplate : String,
      requestsIntervalRange : Range )

  object PageType extends Enumeration {
    val Listing, Album, Artist = Value
  }

  case class Task
    ( pageType : PageType.Value,
      url : String,
      opened : DateTime,
      closed : Option[DateTime] = None,
      priority : Byte = 0 )

  case class Album
    ( genres : Set[Genre],
      tagScores : Map[String, Int],
      name : String,
      tracks : Seq[Track],
      plainTracklist : Seq[String],
      artists : Seq[Artist],
      edition : Option[Edition.Value],
      originalReleaseDate : Option[LocalDate],
      amazonId : String )

  case class Track
    ( genres : Set[Genre],
      amazonId : String,
      artists : Seq[Artist],
      name : String )

  case class Genre
    ( name : String )

  case class Artist
    ( amazonId : String,
      name : String )

  object Edition extends Enumeration {
    val MainAlbums, SinglesAndEPs, LiveAlbums, Imports, LimitedEditions, BoxSets, Compilations = Value
  }

}