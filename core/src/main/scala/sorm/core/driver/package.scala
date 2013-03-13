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
  val preCompiler : PreCompiler[ExecutorInstructions]
  val executor : Executor[ExecutorInstructions, ExecutorResult]
  val parser : Parser[ExecutorResult]

  val connector : Connector
}

// A driver part executed by macros on compile time, which transforms the 
// driver-agnostic Action into the driver-specific instructions, thus 
// significantly reducing the work to be done at runtime.
// In case of RDBMS drivers it makes sense to produce the SQL with it.
trait PreCompiler[ Output ] {

}

trait Parser[ Input ] {

}

trait Executor[ Instructions, Result ] {
  
}

trait Connector {
  def withConnection[ ResultT ]( f : Connection => ResultT ) : ResultT
}
