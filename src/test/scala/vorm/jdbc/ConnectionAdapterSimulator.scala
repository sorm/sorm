package vorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.{Connection, Statement => JStatement}

class ConnectionAdapterSimulator
  extends ConnectionAdapter(null)
  with Logging
  {

    override def executeUpdateAndGetGeneratedKeys
      ( stmt : Statement )
      : List[IndexedSeq[Any]] 
      = {
        logger.info(stmt.toString)
        List(Vector(777l))
      }

    override def executeUpdate
      ( stmt : Statement )
      : Int = {
        logger.info(stmt.toString)
        1
      }

  }