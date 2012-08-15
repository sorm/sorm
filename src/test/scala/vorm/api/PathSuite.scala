package vorm.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import samples._

@RunWith(classOf[JUnitRunner])
class PathSuite extends FunSuite with ShouldMatchers {
  
  import Path._
  
  test("pathAndRemainder failure"){
    pending
  }
  test("pathAndRemainder braced parsing"){
    partAndRemainder("(asdf)") should be === (Part.Braced("asdf"), "")
    partAndRemainder("(asdf).sdf") should be === (Part.Braced("asdf"), ".sdf")
    partAndRemainder("(342).sdf") should be === (Part.Braced("342"), ".sdf")
  }
  test("pathAndRemainder dotted parsing"){
    partAndRemainder("sdf") should be === (Part.Dotted("sdf"), "")
    partAndRemainder("sdf.dksfje") should be === (Part.Dotted("sdf"), ".dksfje")
    partAndRemainder(".sdf.dksfje") should be === (Part.Dotted("sdf"), ".dksfje")
  }
}