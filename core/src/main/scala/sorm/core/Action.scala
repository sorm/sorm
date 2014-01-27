package sorm.core

case class Action[ result ]( f: expressions.Runner[_] => result )
object Action extends scalaz.syntax.ToMonadOps {
  implicit val monad = new scalaz.Monad[Action] {
    def point[a](a: => a) = Action(_ => a)
    def bind[a, b](a: Action[a])(k: a => Action[b]) = {
      Action{ runner =>
        val aResult = a.f(runner)
        val bAction = k(aResult)
        bAction.f(runner)
      }
    }
  }
  implicit class RunOps[r](self: Action[r]) {
    def runOn( runner: Runner ) = runner.run(self)
    def runAsTransactionOn( runner: Runner ) = runner.runAsTransaction(self)
  }
  
  trait Runner {
    def run[ result ]( action: Action[ result ] ): result
    def runAsTransaction[ result ]( action: Action[ result ] ): result
  }
}

