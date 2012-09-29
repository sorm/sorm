package sorm.newMappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

class SeqMapping
  ( protected val reflection : Reflection,
    protected val membership : Option[Membership],
    protected val settings : Map[Reflection, EntitySettings],
    protected val driver : Driver )
  extends TableMapping
  with Querying
  with Parsing {

  lazy val item 
    = Mapping( reflection.generics(0), Membership.SeqItem(this), settings, driver )
  //  ambiguity with direct reference to for instance Entity
  // lazy val masterReferenceMappings
  //   = containerTableMapping.toStream
  //       .flatMap(_.primaryKeyMappings)
  //       .map(m => new ValueMapping(m.reflection, Membership.ContainerReferenceKey))

  // protected def parseResultSet ( rs : ResultSet )
  // protected def fetch ( asql : AbstractSql ) : _
  // protected def fetch ( rs : ResultSet ) : Seq[_]
  //   = {
  //     rs.next()
  //     def propertyValue ( p : String )
  //       = 
  //   }
  protected def parseRows ( rows : Stream[Map[String, _]] ) : Vector[_]
    = rows.map(valueFromOwnRow).toVector

  private def valueFromOwnRow ( row : Map[String, _] ) : Any
    = item.valueFromContainerRow(row, primaryKey.zipBy(row).toMap)

  protected def valueFromContainerRow ( row : Map[String, _], pk : Map[String, _] )
    = fetchByContainerPrimaryKey(pk).get

  protected def mappings = item +: Stream()
  protected def primaryKey = ???
  protected def isMasterTable = false
}