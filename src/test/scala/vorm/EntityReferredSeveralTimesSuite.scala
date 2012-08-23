package vorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import api._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import extensions._

import samples._
import com.codahale.logula.Logging
import org.apache.log4j.Level

@RunWith(classOf[JUnitRunner])
class EntityReferredSeveralTimesSuite extends FunSuite with ShouldMatchers {
  // TODO
  ???
}
object EntityReferredSeveralTimesSuite {
  case class A (b : B, seqOfBs : Seq[B])
  case class B (name : String)
}