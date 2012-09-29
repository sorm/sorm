package sorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.ResultSet

class ConnectionAdapterSimulator
  extends ConnectionAdapter(null)
  with Logging
  {
    override def executeQuery
      [ T ]
      ( s : Statement )
      ( parse : Stream[Map[String, Any]] => T = (_ : Stream[Map[String, Any]]).map(_.values).toList)
      : T
      = {
        println(s.toString)
        ???
      }

    override def executeUpdateAndGetGeneratedKeys
      ( stmt : Statement )
      : List[IndexedSeq[Any]] 
      = {
        println(stmt.toString)
        List(Vector(777l))
      }

    override def executeUpdate
      ( stmt : Statement )
      : Int = {
        println(stmt.toString)
        1
      }

  }