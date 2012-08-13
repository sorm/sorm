package vorm.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import samples._

@RunWith(classOf[JUnitRunner])
class EntitySuite extends FunSuite with ShouldMatchers {
  import EntitySuite._

  //  this is questionable
  // test("self reference validation"){
  //   evaluating { 
  //     Entity[SelfReferringEntity] 
  //     } should produce [IllegalArgumentException]
  //   }

  }
object EntitySuite {
  case class SelfReferringEntity
    ( a : Seq[Option[(SelfReferringEntity, Int)]] )
  case class SelfReferringEntity2
    ( a : SelfReferringEntity2 )
  }