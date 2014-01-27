package sorm.core

import language.reflectiveCalls
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import sorm._, core._, expressions._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ExpressionsTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, bs: Seq[B])
  case class B(a: Int)

  class API extends builders.API {
    protected val expressionsRunner: Runner = ???

    protected def selectParser[a] = new ResultParser[Iterable[a with core.api.Persisted]] {
      type Source = java.sql.ResultSet
      def parse(source: Source) = ???
    }
  }

  val api = new API

  val x = api.from[A].offset(2).select

}
