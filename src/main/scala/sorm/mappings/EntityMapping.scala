package sorm.mappings

import sext.Sext._
import sorm._
import core._
import persisted.Persisted
import reflection.Reflection

class EntityMapping 
  ( val reflection : Reflection, 
    val membership : Option[Membership], 
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends MasterTableMapping {

  lazy val properties
    = reflection.properties.map{case (n, r) => n -> Mapping(r, Membership.EntityProperty(n, this), settings, driver)}
  lazy val mappings // todo: add id
    = properties.values.toStream
  lazy val primaryKeyColumns
    = id.column +: Stream()
  lazy val id
    = new ValueMapping(Reflection[Int], Some(Membership.EntityId(this)), settings, driver)

  override def parseRows ( rows : Stream[String => Any] ) : Any
    = rows
        .headOption
        .map( row => Persisted(
          properties.mapValues( _.valueFromContainerRow(row) ),
          row("id").asInstanceOf[Long],
          reflection
        ) )
        .get

  def delete ( value : Any ) {
    value match {
      case value : Persisted =>
        ("id" -> value.id) $ (Stream(_)) $ (tableName -> _) $$ driver.delete
      case _ =>
        throw new SormException("Attempty to delete an unpersisted entity")
    }
  }

  def valuesForContainerTableRow ( value : Any )
    = value match {
        case value : Persisted =>
          ( memberName + "$id" -> value.id ) +: Stream()
        case _ =>
          throw new SormException("Attempt to refer to an unpersisted entity")
      }

  def save ( value : Any ) : Persisted
    = {
      val propertyValues = properties.map{ case (n, m) => (n, m, reflection.propertyValue(n, value.asInstanceOf[AnyRef])) }.toStream
      val rowValues = propertyValues.flatMap{ case (n, m, v) => m.valuesForContainerTableRow(v) }

      value match {
        case value : Persisted =>
          val pk = Stream(value.id)
          driver.update(tableName, rowValues, pk $ (primaryKeyColumnNames zip _))
          propertyValues.foreach{ case (n, m, v) => m.update(v, pk) }
          value
        case _ =>
          val id = driver.insert(tableName, rowValues).ensuring(_.length == 1).head.asInstanceOf[Long]
          propertyValues.foreach{ case (n, m, v) => m.insert(v, Stream(id)) }
          Persisted( propertyValues.map(t => t._1 -> t._2).toMap, id, reflection )
      }
    }

}