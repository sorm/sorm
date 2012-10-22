package sorm.tableSorters

import sorm._
import connection.Connection
import reflection._
import mappings._
import sext._
import util.Random

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DropTest extends FunSuite with ShouldMatchers {


  import samples.ArtistModel._
  import Drop._

  val settings
    = Map(
      Reflection[Artist] →
      EntitySettings(),

      Reflection[Style] →
      EntitySettings(),

      Reflection[Name] →
      EntitySettings(indexes = Set(Seq("value"))),

      Reflection[Locale] →
      EntitySettings(indexes = Set(Seq("code")))
    )

  val connection
    = Connection("jdbc:h2:mem:test", "", "")
  val mappings
    = settings.keys.map {r => r -> new EntityMapping(r, None, settings, connection)}.toMap


  test("sort simulation") {
    (1 to 200).foreach{ _ =>
      val src
        = Random.shuffle(allTables(mappings.values))
      val result
        = sort(src)
      val indexes
        = result.view.map{_.name}.zipWithIndex.toMap

      indexes("artist$names") should be < (indexes("artist"))
      indexes("artist$names") should be < (indexes("name"))
      indexes("artist$styles") should be < (indexes("artist"))
      indexes("artist$styles") should be < (indexes("style"))
      indexes("name") should be < (indexes("locale"))
      indexes("style$names") should be < (indexes("style"))
      indexes("style$names") should be < (indexes("name"))
    }

  }
}