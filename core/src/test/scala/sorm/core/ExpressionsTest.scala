package sorm.core

import language.reflectiveCalls
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import sorm._, core._, expressions._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ExpressionsTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, bs: Seq[B])
  case class B(a: Int)

  abstract class Driver extends builders.API[ Driver ] with members.API {
    protected val expressionsRunner: Runner[ Driver ] = ???
  }
  object Driver {
    implicit def select
      [ a ]
      =
      new ResultParser[ Driver, Iterable[ a with api.Persisted ] ] {
        type Source = java.sql.ResultSet
        def parse(source: Source) = {
          ???
        }
      }
  }

  object instance extends Driver {
    protected val members = membersFromTuple(member[A], member[B])
  }

  val x = instance.from[A].offset(2).select

}
