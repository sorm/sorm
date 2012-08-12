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
  test("pathAndRemainder key parsing"){
    partAndRemainder("(asdf)") === (Part.Key("asdf"), "")
    partAndRemainder("(asdf).sdf") === (Part.Key("asdf"), ".sdf")
  }
  test("pathAndRemainder index parsing"){
    partAndRemainder("(23)") === (Part.Index(23), "")
    partAndRemainder("(23).sdf") === (Part.Index(23), ".sdf")
  }
  test("pathAndRemainder property parsing"){
    partAndRemainder("sdf") === (Part.Property("sdf"), "")
    partAndRemainder("sdf.dksfje") === (Part.Property("sdf"), ".dksfje")
    partAndRemainder(".sdf.dksfje") === (Part.Property("sdf"), _)
  }
}