package sorm.core.driver

trait Driver {
  /**
   * 
   */
  type ExecutorInstructions
  /**
   * Unparsed result returned after executing the instructions, which should
   * be fed into the parser.
   */
  type ResultSource
  val preCompiler : PreCompiler[ ExecutorInstructions ]
  val executor : Executor[ ExecutorInstructions, ResultSource ]
  val parser : Parser[ ResultSource ]

  type Connection
  val connector : Connector[ Connection ]
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
trait Executor[ Instructions, ResultSource ] {
  /**
   * Delegates the composition with parser to a containing component
   */
  def withResultSource[ Result ]
    ( instructions : Instructions )
    ( f : ResultSource => Result )
    : Result
}

/**
 * @tparam Input In case of JDBC should be a ResultSet
 */
trait Parser[ Input ] {
  def parse[ Output ]( input : Input ) : Output
}

trait Connector[ Connection ] {
  def withConnection[ Result ]( f : Connection => Result ) : Result
}
