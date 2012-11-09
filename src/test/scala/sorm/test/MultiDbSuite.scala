package sorm.test

import sorm._
import core._
import jdbc._
import samples._
import mappings._
import sext._, embrace._
import org.scalatest.FunSuite
import com.weiglewilczek.slf4s.Logging

trait MultiDbSuite extends FunSuite with Logging {

  private def url ( t : DbType )
    = t match {
        case DbType.H2 => "jdbc:h2:mem:test"
        case DbType.Mysql => "jdbc:mysql://localhost/test"
        case DbType.Sqlite => "jdbc:sqlite::memory:"
        case DbType.Hsqldb => "jdbc:hsqldb:mem:testdb"
        case _ => ???
      }

  private def name ( t : DbType )
    = t.toString

  private def db ( entities : Traversable[Entity], t : DbType, poolSize : Int )
    = new Instance(entities, url(t), poolSize = poolSize, initMode = InitMode.DropAllCreate)

  def withDbs1
    ( entities : Traversable[Entity], 
      poolSizes : Seq[Int] = 1 :: 6 :: Nil,
      dbTypes : Seq[DbType] = DbType.Mysql :: DbType.H2 :: Nil )
    ( f : (Instance, String) => Unit )
    = for { dbType <- dbTypes ; poolSize <- poolSizes } {
        val env = name(dbType) + ":" + poolSize
        logger.info("Testing " + env)
        f(db(entities, dbType, poolSize), env)
      }

  def withDbs2
    ( entities : Traversable[Entity], 
      poolSizes : Seq[Int] = 1 :: 6 :: Nil,
      dbTypes : Seq[DbType] = DbType.Mysql :: DbType.H2 :: Nil )
    ( f : (Instance, String => Unit => Unit) => Unit )
    = for { dbType <- dbTypes ; poolSize <- poolSizes } {
        val env = name(dbType) + ":" + poolSize
        logger.info("Testing " + env)
        f(
          db(entities, dbType, poolSize),
          n => f => super.test(n + " - " + env)(f)
        )
      }

}
