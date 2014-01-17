package sorm.core.driver

import sorm.core.util._

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

  protected val compiler : Compiler[ Instructions, CompiledInstructions ]
  protected val executor : Executor[ CompiledInstructions, Connection, ResultResource ]
  protected val parser : Parser[ ResultResource ]
  protected val connector : Connector[ Connection ]

  private val memoCompile = memo(compiler.compile)
  
  /**
   * Execute driver-agnostic instructions and get the parsed result.
   *
   * @tparam output
   * @param instructions
   * @return Parsed result
   */
  final def execute
    [ input, output ]
    ( instructions : Instructions, input : input )
    : output
    = {
      val compiledInstructions = memoCompile( instructions )
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
   * @tparam output
   * @param instructions
   * @return A future of parsed result
   */
  final def executeAsync
    [ input, output ]
    ( instructions : Instructions, input : input )
    : concurrent.Future[ output ]
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
trait Compiler[ input, output ] {
  def compile( input : input ) : output
}

/**
 * @tparam outputResource In case of JDBC should be a ResultSet
 */
trait Executor[ instructions, connection, outputResource ] {
  /**
   * Execute the instructions on a connection and feed them to the passed in
   * function, release all resources afterwards and return the result of that
   * function.
   * 
   * In case of JDBC this method should handle the proper closing of 
   * `ResultSet`.
   */
  def withResultResource[ input, output ]
    ( instructions : instructions, input : input, connection : connection )
    ( f : outputResource => output )
    : output
}

/**
 * @tparam input In case of JDBC should be a ResultSet
 */
trait Parser[ input ] {
  def parse[ output ]( input : input ) : output
}

/**
 * A manager of database connections. In case of JDBC a connection pool.
 */
trait Connector[ connection ] {
  def withConnection[ result ]( f : connection => result ) : result
}

