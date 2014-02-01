package sorm.core.engine

import sorm._, core._, util._, expressions._
import reflect.runtime.{universe => ru}

trait Engine {

  final type parser[ result ] = Parser[ this.type, result ]{ type source = resultSource }
  final type compiler[ t, v ] = Compiler[ this.type, t, v, compiledTemplate, compiledValues ]
  type compiledTemplate
  type compiledValues
  final type member = members.Member

  final val cachingCompiler = new {

    type key = (member, templates.Action)
    type record = (compiledTemplate, values.Action => compiledValues)

    private val cache = new collection.mutable.WeakHashMap[ key, record ]
    def apply
      [ template <: templates.Action, values <: expressions.values.Action ]
      ( template: template, values: values, compiler: compiler[ template, values ], member: member )
      : (compiledTemplate, compiledValues)
      = {
        def compileRecord = {
          val compiledTemplate = compiler.renderTemplate(template)
          val compiledFunction = (compiler.arrangeValues _).asInstanceOf[expressions.values.Action => compiledValues]
          (compiledTemplate, compiledFunction)
        }
        val key = (member, template)
        val (compiledTemplate, arrangerF) = synchronized( cache.getOrElseUpdate(key, compileRecord) )
        (compiledTemplate, arrangerF(values))
      }
  }

  final def runExpression
    [ result, template <: templates.Action, values <: expressions.values.Action ]
    ( template: template,
      values: values,
      compiler: compiler[ template, values ],
      parser: parser[ result ],
      member: member )
    : result
    = {
      val (compiledTemplate, compiledValues) = cachingCompiler.apply(template, values, compiler, member)
      withConnection{ connection =>
        withResultSource(compiledTemplate, compiledValues, connection){ source =>
          parser.parse(source, member)
        }
      }
    }

  type connection
  /**
   * A manager of database connections. In case of JDBC a connection pool.
   */
  def withConnection[ result ]( f : connection => result ) : result

  type resultSource
  /**
   * Execute the instructions on a connection and feed them to the passed in
   * function, release all resources afterwards and return the result of that
   * function.
   *
   * In case of JDBC this method should handle the proper closing of
   * `ResultSet`.
   */
  def withResultSource[ result ]
    ( template: compiledTemplate, values: compiledValues, connection : connection )
    ( f : resultSource => result )
    : result
}

/**
 * Compiler of expression templates and arranger of associated values.
 */
//  TODO: Probably, better use lambdas.
trait Compiler[ -engine, -inputTemplate, -inputValues, +outputTemplate, +outputValues ]{
  def renderTemplate(input: inputTemplate): outputTemplate
  def arrangeValues(input: inputValues): outputValues
}

trait Parser[ -engine, +result ]{
  type source
  def parse( source: source, member: members.Member ): result
}
