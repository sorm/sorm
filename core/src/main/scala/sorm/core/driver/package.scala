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
  type ExecutorResult
  val preCompiler : PreCompiler[ ExecutorInstructions ]
  val executor : Executor[ ExecutorInstructions, ExecutorResult ]

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
  val parser : Parser[ ResultSource ]
  final def execute[ Result ]( instructions : Instructions ) : Result = {
    val resultSource = openResultSource( instructions )
    val result = parser.parse( resultSource )
    closeResultSource( resultSource )
    result
  }
  protected def openResultSource( instructions : Instructions ) : ResultSource
  protected def closeResultSource( resultSource : ResultSource ) : Unit
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
