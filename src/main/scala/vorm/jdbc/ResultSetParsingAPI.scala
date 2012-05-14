package vorm.jdbc

import java.sql.{ResultSet, ResultSetMetaData}
import collection.mutable.ListBuffer
import worm.query.commons.SQLUtil._
import worm.reflection._
import worm.reflection.ReflectionUtil._
import worm.util.Extensions._

trait ResultSetParsingAPI extends SetupAPI {
  protected def parseAndClose(rs: ResultSet, ref: TypeReflection) = {
    val r = new ListBuffer[Map[String, ReflectedValue[Any, PropertyReflection]]]
    val colsProps = ref.propertyMap.mapKeys(sqlifyName).selectKeys(columnNames(rs.getMetaData))
    while (rs.next()) r += colsProps.map {case (c, p) => p.name -> ReflectedValue(rsValue(rs, c, p), p)}
    rs.close()
    r.toList
  }
  private def columnNames(md: ResultSetMetaData) = {
    1.to(md.getColumnCount).map(md.getColumnName(_))
  }
  private def rsValue(rs: ResultSet, name: String, t: TypeReflection) = {
    t match {
      case _ if t.is[String] => rs.getString(name)
      case _ if t.is[java.math.BigDecimal] => rs.getBoolean(name)
      case _ if t.is[BigDecimal] => throw new UnsupportedOperationException("TODO")
      case _ if t.is[Boolean] => rs.getBoolean(name)
      case _ if t.is[Integer] || t.is[Int] => rs.getInt(name)
      case _ if t.is[Long] => rs.getLong(name)
      case _ if t.is[Float] => throw new UnsupportedOperationException("TODO")
      case _ if t.is[Double] => throw new UnsupportedOperationException("TODO")
      case _ => throw new UnsupportedOperationException("TODO")
    }
  }
}