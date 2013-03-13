package sorm.core.driver

trait Driver {
  /**
   * The instructions compiled to the lowest stage. In case of JDBC these should
   * be Statements.
   */
  type ExecutorInstructions
  /**
   * Unparsed result returned after executing the instructions, which should
   * be fed into the parser.
   */
  type ResultSource
  /**
   * A JDBC or an HTTP connection to the database.
   */
  type Connection

  val preCompiler : PreCompiler[ ExecutorInstructions ]
  val executor : Executor[ ExecutorInstructions, Connection, ResultSource ]
  val parser : Parser[ ResultSource ]
  val connector : Connector[ Connection ]

  final def execute[ Result ]( instructions : ExecutorInstructions ) : Result 
    = connector.withConnection(
        executor.withResultSource( instructions, _ )( parser.parse )
      )
    
}

// A driver part executed by macros on compile time, which transforms the 
// driver-agnostic Action into the driver-specific instructions, thus 
// significantly reducing the work to be done at runtime.
// In case of RDBMS drivers it makes sense to produce the SQL with it.
trait PreCompiler[ Output ] {

}

/**
 * @tparam ResultSource In case of JDBC should be a ResultSet
 */
trait Executor[ Instructions, Connection, ResultSource ] {
  /**
   * Execute the instructions on a connection and feed them to the passed in
   * function, release all resources afterwards and return the result of that
   * function.
   * 
   * In case of JDBC this method should handle the proper closing of 
   * `ResultSet`.
   */
  def withResultSource[ Result ]
    ( instructions : Instructions, connection : Connection )
    ( f : ResultSource => Result )
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
