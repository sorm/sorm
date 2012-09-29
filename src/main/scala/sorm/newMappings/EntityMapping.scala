package sorm.newMappings

import sext.Sext._
import sorm._
import core._
import persisted.Persisted
import reflection.Reflection


class EntityMapping 
  ( protected val reflection : Reflection, 
    protected val membership : Option[Membership], 
    protected val settings : Map[Reflection, EntitySettings],
    protected val driver : Driver ) 
  extends TableMapping 
  with TableMappingQuerying 
  with Parsing {

  protected def parseRows ( rows : Stream[Map[String, _]] ) : Option[_]
    = rows
        .headOption
        .map( row => Persisted(
          properties.mapValues( _.valueFromContainerRow(row, primaryKey.zipBy(row).toMap) ),
          row("id").asInstanceOf[Long],
          reflection
        ) )

  protected def valueFromContainerRow ( row : Map[String, _], pk : Map[String, _] ) : Any
    = fetchByContainerPrimaryKey(pk).get


  protected lazy val properties
    = reflection.properties.map{case (n, r) => n -> Mapping(r, Membership.EntityProperty(n, this), settings, driver)}
  protected lazy val mappings // todo: add id
    = reflection.properties.map{case (n, r) => Mapping(r, Membership.EntityProperty(n, this), settings, driver)}
  protected lazy val primaryKey = "id" +: Stream()
  protected def isMasterTable = true
  
}