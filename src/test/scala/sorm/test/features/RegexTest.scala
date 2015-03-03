package sorm.test.features

import org.scalatest._
import sorm.Entity
import RegexTest._
import sorm.core.DbType
import sorm.test.MultiInstanceSuite

@org.junit.runner.RunWith(classOf[junit.JUnitRunner])
class RegexTest extends FunSuite with ShouldMatchers with MultiInstanceSuite {

  def entities = Set() + Entity[User]()
  override def dbTypes = DbType.H2 :: DbType.Mysql :: DbType.Postgres :: Nil

  instancesAndIds foreach { case (db, dbId) =>

    val a1 = db.save(User("abc1"))
    val a2 = db.save(User("abc2"))
    val a3 = db.save(User("abc3"))

    test(dbId + " - regex"){
      db.query[User].whereRegex("fullname", "^abc").count().should(equal(3))
    }
    test(dbId + " - not regex"){
      db.query[User].whereNotRegex("fullname", "^abc1").count().should(equal(2))
    }

  }

}
object RegexTest {
  case class User(fullname: String)
}