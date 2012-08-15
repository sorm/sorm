package vorm.samples

import vorm._
import api._
import jdbc._
import samples._
import extensions._

object TestingInstance {
  def h2
    ( entities : Entity[_]* )
    = new Instance( entities, "jdbc:h2:mem:test", mode = Mode.DropAllCreate )
  def mysql
    ( entities : Entity[_]* )
    = new Instance( entities, "jdbc:mysql://localhost/test", mode = Mode.DropAllCreate )
  def sqlite
    ( entities : Entity[_]* )
    = new Instance( entities, "jdbc:sqlite::memory:", mode = Mode.DropAllCreate )
  def hsql
    ( entities : Entity[_]* )
    = new Instance( entities, "jdbc:hsqldb:mem:testdb", mode = Mode.DropCreate)

}
