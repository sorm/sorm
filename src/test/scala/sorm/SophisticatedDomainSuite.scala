package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Sorm._
import samples._
import sorm.Sorm.ValidationException
import org.joda.time.DateTime

@RunWith(classOf[JUnitRunner])
class SophisticatedDomainSuite extends FunSuite with ShouldMatchers {
  import SophisticatedDomainSuite._

  test("Correct instantiation doesn't throw exceptions"){
    new Sorm.Instance(
      Sorm.Entity[Settings]() +:
      Sorm.Entity[Task](/* indexes = Set(Seq("opened"), Seq("closed")) */) +:
      Sorm.Entity[Album]() +:
      Sorm.Entity[Track]() +:
      Sorm.Entity[Genre]() +:
      Sorm.Entity[Artist]() +:
      List.empty[Sorm.Entity[_]],
      "jdbc:h2:mem:test"
    )
  }


}
object SophisticatedDomainSuite {
  case class Settings
    ( listingUrlTemplate : String,
      requestsIntervalRange : Seq[Int] )

  object ResponseType extends Enumeration {
    val Listing, Album = Value
  }

  case class Task
    ( responseType : ResponseType.Value,
      url : String,
      opened : DateTime,
      // opened : DateTime = db.fetchDate(),
      closed : Option[DateTime] = None )

  case class Album
    ( genres : Set[Genre],
      tagScores : Map[String, Int],
      name : String,
      tracks : List[Track],
      rawTrackListing : Option[String],
      artist : Option[Artist],
      edition : Option[Edition.Value],
      amazonId : String )

  case class Track
    ( genre : Set[Genre],
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