package vorm.persisted

import PersistedEnabler._
import someOtherPackage.domain.{Genre, Artist}
import scala.App
import reflect.mirror.{Symbol, Type}

object Sandbox extends App {
  type MethodType = {def params: List[Symbol]; def resultType: Type}

  //  val persisted = new Artist("Metallica") with Persisted {
  //    val key = "23lkjds"
  //  }
  //
  //  val persisted2 = new Artist("Nirvana") with Persisted {
  //    val key = "123"
  //  }
  val t = tag[Artist].tpe.members.filter(_.kind == "constructor").head.typeSignature
  val b = t.asInstanceOf[MethodType].params

  //  reflect.mirror.


  PersistedEnabler.toPersisted(
    Artist("Nirvana", Set(Genre("rock"), Genre("grunge"))),
    "some-key"
  )


}


