package vorm

import extensions._
import query._
import structure._
import reflection._

package object select {
  
  sealed case class Statement
    ( template : String,
      values : Seq[(Any, JdbcType)],
      resultSetColumns : Seq[(ddl.Column, mapping.Table)] )
  
  type JdbcType = Int


  def statement
    ( q : Query )
    : Statement
    = {

      //  PLAIN

      val mainTreeTableAlias
        : mapping.Table ⇒ String
        = new collection.mutable.HashMap[mapping.Table, String]() {
            override def default
              ( m : mapping.Table )
              = {
                val n = "t" + size
                update(m, n)
                n
              }
          }

      val filterNodeTableAlias
        : Query.WhereNode ⇒ String
        = ???

      case class Select
        ( columns : Seq[sql.Column],
          from    : sql.From,
          joins   : Seq[sql.Join]      = Nil,
          where   : Option[sql.Where]  = None,
          order   : Option[sql.Order]  = None,
          limit   : Option[sql.Limit]  = None )

      def unfilteredSelect
        ( m : mapping.Table )
        : Select
        = {
          def join
            ( m : mapping.Table,
              s : Select )
            : Select
            = s.copy(
                  columns 
                    = m.resultSetColumns
                        .toStream
                        .map(_.name)
                        .map(sql.Column(_, mainTreeTableAlias(m).some)) ++:
                      s.columns,
                  joins
                    = sql.Join(
                        name = m.tableName,
                        alias = mainTreeTableAlias(m).some,
                        targetTable = mainTreeTableAlias(m.ownerTableMapping),
                        on = m.ownerTableColumnMappings
                      ) +:
                      s.joins
                )

          Select(
              columns 
                = m.resultSetColumns
                    .toStream
                    .map(_.name)
                    .map(sql.Column(_, mainTreeTableAlias(m).some)),
              from 
                = sql.From(m.tableName, mainTreeTableAlias(m).some)
            ) 
            .foldTo( m.subTableMappings )( join )
        }

      def filtered
        ( n : Query.WhereNode,
          s : Select )
        : Select
        = n match {
            case Query.WhereNode.Equals( m : mapping.Seq, v : Seq[_] )
              ⇒ ???
          }


      def statement
        ( select : Select )
        : Statement
        = ???


      unfilteredSelect(q.mapping)
        .foldTo(q.where)(filtered)
        .applyTo(statement)

    }

    


}