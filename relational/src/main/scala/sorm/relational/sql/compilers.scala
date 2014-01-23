package sorm.relational.sql

import sorm._, core._

/**
 * A set of specific compiler implementations.
 * Allows to compose a module with support only for specific types.
 */
object compilers {
  protected type Compiler[template, values] = core.Compiler[template, values, String, Seq[Any]]
  protected val t = templates
  protected val v = values

  trait Statement {
    protected implicit def sqlStatementToStringCompiler
      ( implicit
          selectCompiler: Compiler[t.Statement.Select, v.Statement.Select] )
      =
      new Compiler[t.Statement, v.Statement] {
        def renderTemplate( template: t.Statement ) = template match {
          case template: t.Statement.Select => selectCompiler.renderTemplate(template)
        }
        def arrangeValues( values: v.Statement ) = values match {
          case values: v.Statement.Select => selectCompiler.arrangeValues(values)
        }
      }
  }

  trait Select {
    protected implicit def sqlSelectToStringCompiler
      = ???
  }
  
}

