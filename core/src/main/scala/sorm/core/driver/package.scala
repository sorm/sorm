package sorm.core.driver

/**
 * The driver-agnostic instructions. 
 * 
 * TODO: replace with import of a specific implementation, when it's ready.
 */
trait Instructions

trait Driver {
  /**
   * The instructions compiled to the lowest stage. In case of JDBC these should
   * be Statements.
   */
  type CompiledInstructions
  /**
   * Unparsed result returned after executing the instructions, which should
   * be fed into the parser.
   */
  type ResultResource
  /**
   * A JDBC or an HTTP connection to the database.
   */
  type Connection

  val compiler : Compiler[ CompiledInstructions ]
  val executor : Executor[ CompiledInstructions, Connection, ResultResource ]
  val parser : Parser[ ResultResource ]
  val connector : Connector[ Connection ]

  /**
   * Execute driver-agnostic instructions and get the parsed result.
   *
   * @tparam Result
   * @param instructions
   * @return Parsed result
   */
  final def execute[ Result ]( instructions : Instructions ) : Result = {
    val compiledInstructions = compiler.compile( instructions )
    connector.withConnection(
      executor.withResultResource( compiledInstructions, _ )( parser.parse )
    )
  }

}

/**
 * A compiler of the driver-agnostic instructions into the driver-specific low
 * level instructions. Is supposed to be either somehow triggered by macros or
 * at least to cache the input instructions thus significantly reducing the work
 * to be done at runtime.
 * 
 * In case of JDBC drivers it makes sense to produce SQL with it.
 */
trait Compiler[ Output ] {
  def compile( instructions : Instructions ) : Output
}

/**
 * @tparam ResultResource In case of JDBC should be a ResultSet
 */
trait Executor[ Instructions, Connection, ResultResource ] {
  /**
   * Execute the instructions on a connection and feed them to the passed in
   * function, release all resources afterwards and return the result of that
   * function.
   * 
   * In case of JDBC this method should handle the proper closing of 
   * `ResultSet`.
   */
  def withResultResource[ Result ]
    ( instructions : Instructions, connection : Connection )
    ( f : ResultResource => Result )
    : Result
}

/**
 * @tparam Input In case of JDBC should be a ResultSet
 */
trait Parser[ Input ] {
  def parse[ Output ]( input : Input ) : Output
}

/**
 * A manager of database connections. In case of JDBC a connection pooler.
 */
trait Connector[ Connection ] {
  def withConnection[ Result ]( f : Connection => Result ) : Result
}
