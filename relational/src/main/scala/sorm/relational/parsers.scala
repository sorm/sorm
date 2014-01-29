package sorm.relational.parsers

import sorm._, core._, expressions._

trait Select[ driver ] {
  implicit def select
    [ select <: templates.Select ]
    ( implicit memberResolver: templates.Select.MemberResolver[select] )
    =
    new templates.Action.ResultParser[driver, templates.Action.Select[select]] {
      type Source = java.sql.ResultSet
      type Result = Iterable[ memberResolver.Root with api.Persisted ]
      def parse(source: Source) = {
        ???
      }
    }
}
