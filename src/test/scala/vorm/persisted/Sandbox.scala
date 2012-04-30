package vorm.persisted

import someOtherPackage.domain.Artist
import PersistedEnabler._

object Sandbox extends App {


  //  val persisted = new Artist("Metallica") with Persisted {
  //    val key = "23lkjds"
  //  }
  //
  //  val persisted2 = new Artist("Nirvana") with Persisted {
  //    val key = "123"
  //  }

  PersistedEnabler.toPersisted(new Artist("Nirvana"), "ASS")


}


