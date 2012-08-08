package vorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.{Connection, Statement => JStatement}

class ConnectionAdapterSimulator
  extends ConnectionAdapter(null)
  with Logging
  {
    override def executeQuery
      ( s : Statement )
      : ResultSet
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