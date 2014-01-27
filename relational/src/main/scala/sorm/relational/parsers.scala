package sorm.relational.parsers

import sorm._, core._, expressions._

trait Select {
  implicit def select
    [ a ]
    =
    new ResultParser[ Iterable[ a with api.Persisted ] ] {
      type Source = java.sql.ResultSet
      def parse(source: Source) = {
        ???
      }
    }
}
