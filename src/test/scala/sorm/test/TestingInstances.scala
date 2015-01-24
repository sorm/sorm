package sorm.test

import java.sql.SQLException

import sorm._, core._
import sext._, embrace._

object TestingInstances {
  private def url ( t : DbType )
    = t match {
        case DbType.H2 => "jdbc:h2:mem:test"
        case DbType.Mysql => "jdbc:mysql://localhost/test"
        case DbType.Sqlite => "jdbc:sqlite::memory:"
        case DbType.Hsqldb => "jdbc:hsqldb:mem:test"
        case DbType.Derby => "jdbc:derby:memory:test;create=true"
        case DbType.Postgres => "jdbc:postgresql:test"
        case _ => ???
      }

  private def name ( t : DbType )
    = t.toString

  def instance ( entities : Traversable[Entity], t : DbType, poolSize : Int = 1 )
    = {
      def createInstance = t match {
        case DbType.Postgres =>
          new Instance(entities, url(t), "postgres", poolSize = poolSize, initMode = InitMode.DropAllCreate)
        case _ =>
          new Instance(entities, url(t), poolSize = poolSize, initMode = InitMode.DropAllCreate)
      }
      try createInstance catch {
        case e : java.sql.SQLException =>
          throw new SormException("Failed connecting to DB of type " ++ t.toString)
      }
    }

  def instances
    ( entities : Traversable[Entity],
      poolSizes : Seq[Int] = 1 :: 6 :: Nil,
      dbTypes : Seq[DbType] = DbType.H2 :: DbType.Mysql :: DbType.Hsqldb :: DbType.Postgres :: Nil )
    : Stream[(Instance, String)]
    = dbTypes.toStream.flatMap(t => poolSizes.map(t -> _))
        .map{ case (t, s) => instance(entities, t, s) -> (name(t) + ":" + s) }
}
