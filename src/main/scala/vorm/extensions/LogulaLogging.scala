package vorm.extensions

import com.codahale.logula.{Log, Logging}

trait LogulaLogging extends Logging {
  override protected lazy val log : Log
    = Log.forName( getClass.getName )
}
