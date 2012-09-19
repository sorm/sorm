package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import sorm.samples.TestingInstance
import sorm.Sorm.{Instance, Entity}
import util.Random
import org.joda.time.{DateTime, LocalDate}
import sext.Sext._

object DeadlockTest {
  case class Album
    ( asin : String,
      name : String,
      artists : Seq[Artist],
      artistNames : Seq[String],
      format : String,
      tracklist : Seq[String],
      mp3s : Seq[Mp3],
      genres : Set[Genre],
      categories : Set[String],
      relatedAlbumsAsins : Seq[String],
      relatedArtists : Seq[Artist],
      rating : (Int, Int, Int, Int, Int),
      releaseDate : Option[LocalDate],
      originalReleaseDate : Option[LocalDate],
      copyright : Option[Copyright],
      label : Option[String],
      duration : Option[Int] )

  case class Copyright
    ( text : String,
      year : Short )

  case class Mp3
    ( asin : String,
      name : String,
      artists : Seq[Artist],
      duration : Option[Int] )

  case class Genre
    ( name : String )

  case class Artist
    ( asin : String,
      names : Seq[String] )

  case class Task
    ( asin : String,
      opened : DateTime,
      started : Option[DateTime] = None,
      closed : Option[DateTime] = None,
      failure : Option[String] = None )

}
@RunWith(classOf[JUnitRunner])
class DeadlockTest extends FunSuite with ShouldMatchers {
  import DeadlockTest._

  def instance = TestingInstance.mysql(
    Entity[Task](indexes = Set() + Seq("asin") + Seq("opened") + Seq("closed") + Seq("started")),
    Entity[Artist](unique = Set() + Seq("asin")),
    Entity[Mp3](unique = Set() + Seq("asin")),
    Entity[Copyright](),
    Entity[Genre](unique = Set() + Seq("name")),
    Entity[Album](unique = Set() + Seq("asin"))
  )

  val instances = (0 until 3).map(_ => instance).par
//  val db1 = instance
//  val db2 = instance
//  val db3 = instance
//
//  val a1 = db1.save(A(1))
//  val a2 = db1.save(A(3))
//  val a3 = db1.save(A(0))
//  val a4 = db1.save(A(3000))
//
//  db1.save(B(a2 :: a3 :: Nil))
//  db1.save(B(a3 :: Nil))
//  db1.save(B(a3 :: Nil))
//
//  db2.save(A(684))
//
//  db3.save(A(2))
//  db3.save(B(db3.save(A(4)) :: db3.save(A(6)) :: Nil))
//
  test("Parallel transactions"){
    val ops : Seq[Instance => _]
      = ( (db : Instance) => db.transaction {
            import sorm.Sorm.FilterDsl._
            db.one[Task]
              .filterEqual("closed", None)
              .filter(
                "started" equal None or
                ("started.item" smaller (db.dateTime.minusMinutes(1)))
              )
              .orderAsc("opened")
              .fetch()
              .map(_.copy(started = Some(db.dateTime), failure = None))
              .map(db.save)
            Thread.sleep(2)
          } ) ::
        ( (db : Instance) => db.transaction {
            db.one[Artist].filterEqual("asin", "lsdjkflkjsldf").fetch()
              .map(a => a.copy(names = a.names ++ Nil distinct))
              .map(db.save)
            Thread.sleep(2)
          } ) ::
        ( (db : Instance) => db.transaction {
            db.one[Genre].filterEqual("name", "lsdfj").fetch()
            Thread.sleep(2)
          } ) ::
//        ( (db : Instance) => db.transaction {
//            val savedArtists = mp3.artists map saveArtist
//            db.one[Mp3].filterEqual("asin", mp3.asin).fetch()
//              .map(a => a.copy(artists = a.artists ++ savedArtists distinct))
//              .getOrElse(mp3.copy(artists = savedArtists))
//              .|>(db.save)
//          } ) ::
        Nil

    (0 to 100).flatMap(_ => instances).map(Random.shuffle(ops).head)

  }
  test("Parallel saving"){
    pending
  }

}
