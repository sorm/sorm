package sorm.core

import language.reflectiveCalls
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import sorm._, core._, expressions._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ExpressionsTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, bs: Seq[B])
  case class B(a: Int)


  class Engine extends core.engine.Engine {
    override type compiledTemplate = String
    override type compiledValues = Seq[Any]
    override type connection = Nothing
    override type resultSource = java.sql.ResultSet
    override def withConnection[result](f: (connection) => result) = ???
    override def withResultSource[result](template: compiledTemplate, values: compiledValues, connection: connection)(f: resultSource => result) = ???
  }
  object Engine {
    implicit def selectParser
      [ a ]
      =
      new engine.Parser[Engine, Iterable[ a with api.Persisted ]]{
        override type source = java.sql.ResultSet
        override def parse(source: source, member: members.Member) = ???
      }
  }
  abstract class API extends builders.API with members.API {
    override protected val engine = new Engine
  }
  object instance extends API {
    protected val members = membersFromTuple(member[A], member[B])
  }

//  val x = instance.from[A].limit(2).select

}
