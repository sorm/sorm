package sorm.test

import org.scalatest.{SequentialNestedSuiteExecution, Suite, BeforeAndAfterAll}
import sorm.Entity
import sorm.core.DbType

trait MultiInstanceSuite extends Suite with BeforeAndAfterAll with SequentialNestedSuiteExecution {

  def entities : Traversable[Entity]
  def poolSizes = 1 :: 6 :: Nil
  def dbTypes = DbType.H2 :: DbType.Mysql :: DbType.Hsqldb :: DbType.Postgres :: Nil
  lazy val instancesAndIds = TestingInstances.instances( entities, poolSizes, dbTypes )

  override protected def afterAll() {
    instancesAndIds.unzip._1.foreach(_.close())
  }

}
