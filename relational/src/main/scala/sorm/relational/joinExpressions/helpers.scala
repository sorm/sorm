package sorm.relational.joinExpressions

/**
 * Completely runtime stuff.
 */
object helpers {

  import sorm.relational._
  import templates._
  import sorm.core._
  import reflect.runtime.{universe => ru}
  import rules._

  def column( mapping: Mapping ): Option[Column] = {
    for {
      name <- mapping.memberNameBasis
      from <- mapping.parent.flatMap(this.from)
    }
    yield Column(name, from)
  }

  def from( mapping: Mapping ): Option[From] = {
    mapping.parent match {
      case None => mapping.tableName.map(From.Root)
      case Some(parentMapping) => {
        def bindingsFromParent = mapping.foreignKeyForParent.map(_.bindings.map(_.swap))
        def bindingsToParent = mapping.foreignKeyToParent.map(_.bindings)
        for {
          parent <- from(parentMapping)
          name <- mapping.tableName
          bindings <- bindingsFromParent.orElse(bindingsToParent)
        }
        yield From.Join(name, parent, bindings)
      }
    }
  }


}
