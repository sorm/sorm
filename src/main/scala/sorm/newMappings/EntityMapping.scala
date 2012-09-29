package sorm.newMappings

import sext.Sext._
import sorm._
import core._
import persisted.Persisted
import reflection.Reflection


class EntityMapping 
  ( val reflection : Reflection, 
    val membership : Option[Membership], 
    val settings : Map[Reflection, EntitySettings],
    val driver : Connection )
  extends MasterTableMapping {

  override def parseRows ( rows : Stream[String => Any] ) : Option[Any]
    = rows
        .headOption
        .map( row => Persisted(
          properties.mapValues( _.valueFromContainerRow(row) ),
          row("id").asInstanceOf[Long],
          reflection
        ) )

  lazy val properties
    = reflection.properties.map{case (n, r) => n -> Mapping(r, Membership.EntityProperty(n, this), settings, driver)}
  lazy val mappings // todo: add id
    = reflection.properties.map{case (n, r) => Mapping(r, Membership.EntityProperty(n, this), settings, driver)}.toStream
  lazy val primaryKeyColumns
    = id.column +: Stream()
  lazy val id
    = new ValueMapping(Reflection[Int], Some(Membership.EntityId(this)), settings, driver)

}