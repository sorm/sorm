package sorm.core

import language.reflectiveCalls
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import sorm._, core._, expressions._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ExpressionsTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, bs: Seq[B])
  case class B(a: Int)

  class Driver extends builders.API[ Driver ] {
    protected val expressionsRunner: Runner[ Driver ] = ???
  }
  object Driver {
    implicit def select
      [ select <: templates.Select ]
//      ( implicit memberResolver: templates.Select.MemberResolver[select] )
      =
      new templates.Action.ResultParser[Driver, templates.Action.Select[select]] {
        type Source = java.sql.ResultSet
//        type Result = Iterable[ memberResolver.Root with core.api.Persisted ]
        def parse(source: Source) = {
          ???
        }
      }
  }

  val driver = new Driver

  val x = driver.from[A].offset(2).select

}
