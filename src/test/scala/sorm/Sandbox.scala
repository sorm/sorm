package sorm

import api._
import jdbc._
import samples._
import extensions.Extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level
import java.sql.DriverManager

object Sandbox extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("sorm.jdbc.ConnectionAdapter") = Level.TRACE
  }

  case class A ( b : B )
  case class B ( seqOfSeqsOfInts : Seq[Seq[Int]])

  val db = TestingInstance.mysql( Entity[A](), Entity[B]() )
  val b1 = db.save(B( Seq() ))
  val b2 = db.save(B( Seq(Seq(2, 9, 3)) ))
  val b3 = db.save(B( Seq(Seq(4)) ))
  val b4 = db.save(B( Seq() ))
  db.save(A( b1 ))
  db.save(A( b2 ))
  db.save(A( b2 ))
  db.save(A( b3 ))
  db.save(A( b4 ))

  import sorm.query.AbstractSqlComposition._
  import sorm.abstractSql.StandardSqlComposition._
  import sorm.sql.StandardRendering._

  db.query[A]
    .filterEquals("b.seqOfSeqsOfInts.item", Seq(2,9,3))
    .order("b.seqOfSeqsOfInts.item.item")
    .fetchAll()
    .prettyString.trace()


//  db.connection.executeQuery(
//    Statement(
//      """
//      SELECT
//        a.id
//        FROM
//          a
//          AS a
//        LEFT JOIN
//          a$a
//          AS b
//          ON b.p$id = a.id
//        LEFT JOIN
//          a$a
//          AS c
//          ON c.p$id = a.id
//        GROUP BY a.id
//        HAVING COUNT(DISTINCT b.i) = 0 AND
//               COUNT(DISTINCT c.i) = 0
//      """
////      """
////      SELECT
////        a.p$id
////        FROM
////          a$a
////          AS a
////        WHERE ( a.i = 2 AND
////                a.v = 3 ) OR
////              ( ( a.i = 1 AND
////                  a.v = 9 ) OR
////                ( a.i = 0 AND
////                  a.v = 2 ) )
////        HAVING COUNT(DISTINCT a.i) = 3
////      """
//    )
//  ) .parseAndClose().prettyString.trace()


}
