package vorm.drop

import vorm._
import structure._
import mapping._
import jdbc._
import vorm.ddl.Table
import extensions._

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

      // ts.sortWith{isSlave}
      sort(ts)
    }

  def statements
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
        .as{sort}
        .map{ "DROP TABLE " + _.name }
        .map{ Statement(_) }



}