package sorm.test.features

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._, core._
import sext._, embrace._
import sorm.test.MultiInstanceSuite

@RunWith(classOf[JUnitRunner])
class FetchWithSqlSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {

  import FetchWithSqlSuite._

  def entities = Set() + Entity[A]()
  override val dbTypes = DbType.H2 :: Nil
  instancesAndIds foreach { case (db, dbId) =>

    val a1 = db.save(A( "A" ))
    val a2 = db.save(A( "B" ))
    val a3 = db.save(A( "C" ))

    test(dbId + " - general") {
      db.fetchWithSql[A]("select id from a where a=?", "B").head
        .should( equal(a2) )
    }
    test(dbId + " - fails on excess columns") {
      intercept[AssertionError] {
        db.fetchWithSql[A]("select id, a from a where a=?", "B")
      }
    }
    test(dbId + " - fails on a wrong single column") {
      intercept[AssertionError] {
        db.fetchWithSql[A]("select a from a where a=?", "B")
      }
    }

  }

}
object FetchWithSqlSuite {

  case class A ( a : String )

}