package sorm.core.driver


trait Driver {
  /**
   * Driver-agnostic instructions to be compiled and cached.
   */
  type Instructions
  /**
   * Result of compiling `Instructions` to the lowest driver-specific state.
   *
   * In case of JDBC it should be something like `PreparedStatement`.
   */
  type CompiledInstructions
  /**
   * Unparsed result returned after executing the instructions, which gets fed
   * to the parser.
   *
   * In case of JDBC should be something like `ResultSet`.
   */
  type ResultResource
  /**
   * Connection to database, e.g. JDBC or HTTP.
   */
  type Connection

  val compiler : Compiler[ Instructions, CompiledInstructions ]
  val executor : Executor[ CompiledInstructions, Connection, ResultResource ]
  val parser : Parser[ ResultResource ]
  val connector : Connector[ Connection ]

  private val memoizedCompiler = new MemoizedCompiler( compiler )
  /**
   * Execute driver-agnostic instructions and get the parsed result.
   *
   * @tparam Result
   * @param instructions
   * @return Parsed result
   */
  final def execute
    [ Input, Result ]
    ( instructions : Instructions, input : Input )
    : Result 
    = {
      val compiledInstructions = memoizedCompiler.compile( instructions )
      connector.withConnection(
        executor.withResultResource
          ( compiledInstructions, input, _ )
          ( parser.parse )
      )
    }

  /**
   * Trigger asynchronous execution and parsing of driver-agnostic instructions
   * and return a [[scala.concurrent.Future]] of parsed result.
   *
   * @tparam Result
   * @param instructions
   * @return A future of parsed result
   */
  final def executeAsync
    [ Input, Result ]
    ( instructions : Instructions, input : Input )
    : concurrent.Future[ Result ]
    = ???

}

/**
 * A compiler of the driver-agnostic instructions into the driver-specific low
 * level instructions. Is supposed to be either somehow triggered by macros or
 * at least to cache the input instructions thus significantly reducing the work
 * to be done at runtime.
 * 
 * In case of JDBC drivers it makes sense to produce SQL with it.
 */
trait Compiler[ Input, Output ] {
  def compile( input : Input ) : Output
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
  def withResultResource[ Input, Result ]
    ( instructions : Instructions, input : Input, connection : Connection )
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

private class MemoizedCompiler
  [ Input, Output ]
  ( compiler : Compiler[ Input, Output ] )
  {
    /**
     * A WeakHashMap will release cache members if memory tightens.
     */
    private val cache = new collection.mutable.WeakHashMap[ Input, Output ]

    /**
     * A memoized `compiler.compile`.
     */
    def compile( input : Input ) =
      cache.getOrElseUpdate( input,
        compiler.compile( input )
      )
  }
