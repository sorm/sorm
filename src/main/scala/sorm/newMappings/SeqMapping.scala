package sorm.newMappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

class SeqMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
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
  def parseRows ( rows : Stream[String => Any] )
    = rows.map(item.valueFromContainerRow).toVector.notEmpty

  lazy val index
    = new ValueMapping(Reflection[Int], Some(Membership.SeqIndex(this)), settings, driver)
  lazy val mappings = item +: Stream()
  lazy val primaryKeyColumns = masterTableColumns :+ index.column
}