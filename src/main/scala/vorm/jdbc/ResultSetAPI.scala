package vorm.jdbc

import collection.mutable.ListBuffer
import java.sql._


class ResultSetAPI(rs: ResultSet) {

  def parseToListsAndClose() = {
    val r = ListBuffer[List[Any]]()

    val md = rs.getMetaData

    val indexTypeList =
      (1 to md.getColumnCount)
        .map(i => (i -> md.getColumnType(i)))
        .toList

    while (rs.next()) r += indexTypeList.map {case (i, t) => value(i, t)}

    rs.close()
    r.toList
  }

  def parseToMapsAndClose() = {
    val r = ListBuffer[Map[String, Any]]()

    val md = rs.getMetaData

    val indexTypeByNameMap =
      (1 to md.getColumnCount)
        .map(i => md.getColumnName(i) -> (i -> md.getColumnType(i)))
        .toMap

    while (rs.next()) r += indexTypeByNameMap.mapValues {case (i, t) => value(i, t)}

    rs.close()
    r.toList
  }

  private def value(i: Int, t: Int): Any = {
    import Types._
    t match {
      case INTEGER => rs.getInt(i)
      case BIGINT => rs.getLong(i)
      case CHAR | VARCHAR => rs.getString(i)
      case _ => throw new NotImplementedError
    }
  }


}
