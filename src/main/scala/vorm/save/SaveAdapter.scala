package vorm.save

import vorm._
import structure._
import mapping._
import jdbc._
import sql._
import save._
import persisted._
import extensions._

trait SaveAdapter extends ConnectionAdapter {
  def saveEntityAndGetIt
    ( v : AnyRef,
      m : EntityMapping )
    : AnyRef with Persisted
    = {
      v match {
        case v : Persisted =>
          // //  delete slaves
          // m.properties.view
          //   .collect{ case p: CollectionTableMapping => p }
          //   .foreach{ deleteAllRows(_, key) }
          ???
        case v =>

          val properties1
            = m.properties.map{ case (n, p) => 
                p -> 
                saveAndGetIt1( m.reflection.propertyValue(n, v), p ) 
              }

          val id : Long
            = insertAndGetGeneratedKeys(
                m.tableName, 
                properties1
                  .flatMap{ case (p, v) => rowValuesForContainerTable(v, p) }
              )
              .head.asInstanceOf[Long]

          val properties2
            = m.properties.map{ case (n, p) =>
                n ->
                saveAndGetIt2( properties1(p), p,
                               Seq( JdbcValue(id, java.sql.Types.BIGINT) ) )
              }

          Persisted( properties2, id, m.reflection )
          // m.persistedReflection
          //   .instantiate( properties2 + ("id" → id) )
      }


      // val values
      //   = 

      // e.mapping.reflection.properties.keys.view


      // val nestedMasterTableMappings
      //   = {
      //     def nestedMasterTableMappings
      //       ( m : Mapping )
      //       : Stream[EntityMapping]
      //       = m match {
      //           case m : EntityMapping => Stream(m)
      //           case m : TableMapping => Stream.empty
      //           case m : HasChildren => m.children flatMap nestedMasterTableMappings
      //           case _ => Stream.empty
      //         }
      //     nestedMasterTableMappings(m)
      //   }


    }

  def saveSeqAndGetIt
    ( v : Seq[_],
      m : SeqMapping,
      containerKey : Iterable[JdbcValue] )
    = {

      val containerKeyCv
        = m.containerTableColumns.view
            .map{ _.name } 
            .zip(containerKey)
            .toMap

      val values
        = v.view.zipWithIndex
            .map{ case (v, i) =>
              val pkCv 
                = containerKeyCv + 
                  ("i" -> JdbcValue(i, java.sql.Types.INTEGER))
              val v1 = saveAndGetIt1(v, m.item)
              insert( m.tableName, pkCv ++ 
                                   rowValuesForContainerTable(v1, m.item) )
              val v2 = saveAndGetIt2(v1, m.item, pkCv.values)
              v2
            }


      values.toIndexedSeq
    }

  def saveMapAndGetIt
    ( v : Map[_, _],
      m : MapMapping,
      containerKey : Iterable[JdbcValue] )
    = {

      val containerKeyCv
        = m.containerTableColumns.view
            .map{ _.name } 
            .zip(containerKey)
            .toMap

      val values
        = v.view.zipWithIndex
            .map{ case ((k, v), i) =>
              val pkCv 
                = containerKeyCv + 
                  ("h" -> JdbcValue(i, java.sql.Types.INTEGER))
              val k1 = saveAndGetIt1(k, m.key)
              val v1 = saveAndGetIt1(v, m.value)
              insert( m.tableName, pkCv ++ 
                                   rowValuesForContainerTable(k1, m.key) ++
                                   rowValuesForContainerTable(v1, m.value) )
              val k2 = saveAndGetIt2(k, m.key, pkCv.values)
              val v2 = saveAndGetIt2(v, m.value, pkCv.values)
              k2 -> v2
            }


      values.toMap
    }

  def saveSetAndGetIt
    ( v : Set[_],
      m : SetMapping,
      containerKey : Iterable[JdbcValue] )
    = {

      val containerKeyCv
        = m.containerTableColumns.view
            .map{ _.name } 
            .zip(containerKey)
            .toMap

      val values
        = v.view.zipWithIndex
            .map{ case (v, i) =>
              val pkCv 
                = containerKeyCv + 
                  ("h" -> JdbcValue(i, java.sql.Types.INTEGER))
              val v1 = saveAndGetIt1(v, m.item)
              insert( m.tableName, pkCv ++ 
                                   rowValuesForContainerTable(v1, m.item) )
              val v2 = saveAndGetIt2(v1, m.item, pkCv.values)
              v2
            }

      values.toSet
    }

  // def saveOptionAndGetIt
  //   ( v : Option[_],
  //     m : OptionMapping,
  //     containerKey : Iterable[JdbcValue] )
  //   = {
  //     val containerKeyCv
  //       = m.containerTableColumns.view
  //           .map{ _.name } 
  //           .zip(containerKey)
  //           .toMap
  //     val values
  //       = v.view
  //           .map{ v =>
  //             val v1 = saveAndGetIt(v, m.item, containerKeyCv.values)
  //             insert( m.tableName, containerKeyCv ++ 
  //                                  rowValuesForContainerTable(v1, m.item) )
  //             v1
  //           }
  //     m.reflection.instantiate(values)
  //   }

  def saveAndGetIt1
    ( v : Any,
      m : Mapping )
    : Any
    = (v, m) match {
        case (v : AnyRef, m : EntityMapping) ⇒ 
          saveEntityAndGetIt(v, m)
        case (v : Traversable[_], m : CollectionTableMapping) ⇒ 
          v
        case (v : Product, m : TupleMapping) ⇒ 
          m.reflection.instantiate(
            (v.productIterator.toStream zip m.items)
              .map{ case (v, m) ⇒ saveAndGetIt1(v, m) }
          )
        case (v : Option[_], m : OptionMapping) ⇒ 
          v.map{ saveAndGetIt1(_, m.item) }
        case (v : Any, m : ValueMapping) ⇒ 
          v
      }

  def saveAndGetIt2
    ( v : Any,
      m : Mapping,
      containerKey : Iterable[JdbcValue] )
    : Any
    = (v, m) match {
        case (v : Persisted, m : EntityMapping) ⇒ 
          v
        case (v : Seq[_], m : SeqMapping) ⇒ 
          saveSeqAndGetIt(v, m, containerKey)
        case (v : Set[_], m : SetMapping) ⇒ 
          saveSetAndGetIt(v, m, containerKey)
        case (v : Map[_, _], m : MapMapping) ⇒ 
          saveMapAndGetIt(v, m, containerKey)
        case (v : Product, m : TupleMapping) ⇒ 
          m.reflection.instantiate(
            (v.productIterator.toStream zip m.items)
              .map{ case (v, m) ⇒ saveAndGetIt2(v, m, containerKey) }
          )
        case (v : Option[_], m : OptionMapping) ⇒ 
          v.map{ saveAndGetIt2(_, m.item, containerKey) }
        case (v : Any, m : ValueMapping) ⇒ 
          v
      }


  def deleteAllRows 
    ( m : CollectionTableMapping,
      containerKey : Iterable[JdbcValue] ) 
    {
      def nestedSlaveTableMappings
        ( m : Mapping )
        : Stream[CollectionTableMapping]
        = m match {
            case m : CollectionTableMapping => Stream(m)
            case m : TableMapping => Stream.empty
            case m : HasChildren => 
              m.children.toStream flatMap nestedSlaveTableMappings
            case _ => Stream.empty
          }

      nestedSlaveTableMappings(m).foreach{ deleteAllRows(_, containerKey) }
      delete( m.tableName, 
              m.containerTableColumns.view.map(_.name) zip containerKey toMap )
    }

  private def delete
    ( table : String,
      where : Map[String, JdbcValue] )
    {
      ???
    }

  private def insert
    ( table : String,
      what : Map[String, JdbcValue] )
    {
      executeUpdate(insertStatement(table, what.toSeq))
    }

  private def insertAndGetGeneratedKeys
    ( table : String,
      what : Map[String, JdbcValue] )
    : Seq[Any]
    = {
      executeUpdateAndGetGeneratedKeys(insertStatement(table, what.toSeq)).head
    }

  private def insertStatement
    ( table : String,
      what : Seq[(String, JdbcValue)] )
    : Statement
    = Statement(
        "INSERT INTO " + quote(table) + "\n" +
        ( "( " + what.view.unzip._1.map{quote}.mkString(", ") + " )\n" + 
          "VALUES\n" +
          "( " + Iterable.fill(what.size)("?").mkString(", ") + " )" )
          .indent(2),
        what.view.unzip._2.toSeq
      )


}
