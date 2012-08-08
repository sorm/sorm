package vorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.{Connection, Statement => JStatement}

trait ConnectionAdapterSimulator
  extends ConnectionAdapter
  with Logging
  {

    override def executeUpdateAndGetGeneratedKeys
      ( stmt : Statement )
      : List[IndexedSeq[Any]] 
      = {
        logger.trace(stmt.toString)
        List(Vector(777))
      }

    override def executeUpdate
      ( stmt : Statement )
      : Int = {
        logger.trace(stmt.toString)
        1
      }

  }