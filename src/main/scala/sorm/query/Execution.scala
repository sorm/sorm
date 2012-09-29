package sorm.query

import sext.Sext._
import sorm._

object Execution {
  import sorm.query.Query._
  import sorm.resultSetParsing.ResultSetParsing._
  import sorm.jdbc._
  import sorm.abstractSql.AbstractSql._
  import sorm.abstractSql._
  import sorm.sql._
  // import sorm.sql.Sql._
  import sorm.sql.StandardRendering._
  // import sorm.structure.mapping.TableMapping

  def execute ( connection : ConnectionAdapter, query : Query ) : Seq[_]
    = {
      val ids : Seq[Long]
        = ???

      ids map ( getById(query.mapping, _) )
    }


  // def getByParentPrimaryKey
  //   ( connection : ConnectionAdapter,
  //     mapping : TableMapping,
  //     primaryKey : Seq[(String, _)] )
  //   : _
  //   = ???

  private val mappingExtension1 = memo { new MappingExtension(_) }

  implicit def mappingExtension ( m : Mapping ) = mappingExtension1(m)

  





  // def getByContainerId 
  //   ( connection : ConnectionAdapter,
  //     mapping : TableMapping,
  //     id : Long ) 
  //   : Seq[_]
  //   = {
  //     val containerTable = mapping.containerTableMapping map (_.tableName) map (Table(_))
  //     val table = Table(mapping.tableName, containerTable.map(Parent(_, mapping.bindingsToContainerTable)))
  //     val columns = mapping.columns.view.map(_.name).map(Column(_, table))
  //     val asql = Select(columns, containerTable.map(Comparison(_, "id", Equal, id)))
  //     val stmt = stmt( StandardSqlComposition.sql(asql) )
  //     val columnIndexes = mapping.columns.view.zipWithIndex.map{case (c, i) => (mapping, c) -> i }.toMap
  //     connection.executeQuery(stmt)(parseInstances(_, mapping, columnIndexes))
  //   }
      

  // def getByAncestorId 
  //   ( mapping : TableMapping, 
  //     ancestor : TableMapping,
  //     containerId : Long ) 
  //   : _
  //   = {
  //     ???
  //   }
}
