package vorm.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PathSuite extends FunSuite with ShouldMatchers {
  
  import Path._

  test("pathAndRemainder failure"){
    pending
  }
  test("pathAndRemainder braced parsing"){
    partAndRemainder("(asdf)") === (Part.Braced("asdf"), "")
    partAndRemainder("(asdf).sdf") === (Part.Braced("asdf"), ".sdf")
    partAndRemainder("(342).sdf") === (Part.Braced("342"), ".sdf")
  }
  test("pathAndRemainder dotted parsing"){
    partAndRemainder("sdf") === (Part.Dotted("sdf"), "")
    partAndRemainder("sdf.dksfje") === (Part.Dotted("sdf"), ".dksfje")
    partAndRemainder(".sdf.dksfje") === (Part.Dotted("sdf"), ".dksfje")
  }
}