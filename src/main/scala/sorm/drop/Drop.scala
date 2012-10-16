package sorm.drop

import sorm._
import structure._
import mapping._
import jdbc._
import sorm.ddl.Table
import sext._

object Drop {

  def ddl
    ( ms : Iterable[TableMapping] )
    : String
    = statements(ms)
        .map{_.sql + ";"}
        .mkString("\n")


  def sort
    ( ts : Stream[Table] )
    = {
      val tableByName 
        = ts.map{ t => t.name -> t }.toMap 

      def masters
        ( t : Table )
        : Set[Table]
        = t.foreignKeys.view.map{_.table}.map{tableByName}
            .flatMap{t => masters(t) + t}
            .toSet

      def isSlave
        ( l : Table,
          r : Table )
        = masters(l)(r)

      def sort
        ( xs : Stream[Table] )
        : Stream[Table]
        = if( xs.isEmpty ) xs
          else
            sort( xs.tail.filter{isSlave(_, xs.head)} ) #:::
            xs.head #::
            sort( xs.tail.filterNot{isSlave(_, xs.head)} )

      sort(ts)
    }

  def allTables
    ( ms : Iterable[TableMapping] )
    = ms.flatMap{ m ⇒
          def queue
            ( m : TableMapping )
            : Set[TableMapping]
            = m.nestedTableMappings.flatMap{queue} + m
          queue( m )
        }
        .toStream
        .map{ _.table }
        .distinct

  def statements
    ( ms : Iterable[TableMapping] )
    = allTables(ms)
        .as{sort}
        .map{ "DROP TABLE " + _.name }
        .map{ Statement(_) }



}