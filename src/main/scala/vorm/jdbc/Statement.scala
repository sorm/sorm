package vorm.jdbc

import worm.reflection._

case class Statement(
  sql: String,
  data: Seq[(Any, TypeReflection)]
  //  could also store return types for parsing of resultset
) {
  override def toString = sql + "\n" + data.map {case (v, r) => v + "(" + r.typeName + ")"}.mkString(", ")
}