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
    val connection : Connection )
  extends MasterTableMapping {

  override def parseRows ( rows : Stream[String => Any] ) : Any
    = rows
        .headOption
        .map( row => Persisted(
          properties.mapValues( _.valueFromContainerRow(row) ),
          row("id").asInstanceOf[Long],
          reflection
        ) )
        .get

  lazy val properties
    = reflection.properties.map{case (n, r) => n -> Mapping(r, Membership.EntityProperty(n, this), settings, connection)}
  lazy val mappings // todo: add id
    = reflection.properties.map{case (n, r) => Mapping(r, Membership.EntityProperty(n, this), settings, connection)}.toStream
  lazy val primaryKeyColumns
    = id.column +: Stream()
  lazy val id
    = new ValueMapping(Reflection[Int], Some(Membership.EntityId(this)), settings, connection)

}