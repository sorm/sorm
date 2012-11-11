package sorm.test

import org.scalatest.{SequentialNestedSuiteExecution, Suite, BeforeAndAfterAll}
import sorm.Entity

trait MultiInstanceSuite extends Suite with BeforeAndAfterAll with SequentialNestedSuiteExecution {

  def entities : Traversable[Entity]
  def poolSizes = 1 :: 6 :: Nil
  lazy val instancesAndIds = TestingInstances.instances( entities, poolSizes )

  override protected def afterAll() {
    instancesAndIds.unzip._1.foreach(_.close())
  }

}
