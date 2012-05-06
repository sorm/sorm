package vorm

import vorm.reflection._
import tools.nsc.interpreter.IMain
import tools.nsc._

package object persisted {

  def toPersisted[T <: AnyRef : TypeTag](instance: T, id: String): T with Persisted = {
    val t = tpe[T]

    val args =
      id ::
      t.constructors.head.arguments.map(_.name)
        .map(t.propertyValue(_, instance))

    persistedClass[T](t)
      .getConstructors.head
      .newInstance(args.asInstanceOf[List[Object]]: _*)
      .asInstanceOf[T with Persisted]

  }


  private lazy val interpreter = {
    val settings = new Settings()
    settings.usejavacp.value = true
    new IMain(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  }

  private var generateNameCounter = 0l

  private def generateName() = synchronized {
    generateNameCounter += 1
    "PersistedAnonymous" + generateNameCounter
  }

  private val persistedClassCache =
    collection.mutable.Map[Type, Class[_]]()

  private def persistedClass[T](t: Type): Class[T with Persisted] =
    try persistedClassCache(t).asInstanceOf[Class[T with Persisted]]
    catch {
      case _ =>
        val name = generateName()

        val code = {
          val sourceArgs =
            t.constructors.head.arguments

          val sourceArgSignatures =
            sourceArgs.map(a => a.name + ": " + a.t.signature)

          val newArgSignatures =
            "val key: String" :: sourceArgSignatures

          val copyMethodArgSignatures =
            sourceArgs.map(a => a.name + ": " + a.t.signature + " = " + a.name)

          val copyMethodInstantiationArgs =
            "key" :: sourceArgs.map(_.name)

          """
          class """ + name + """(""" + newArgSignatures.mkString(", ") + """)
            extends """ + t.signature + """(""" + sourceArgSignatures.mkString(", ") + """)
            with """ + tpe[Persisted].signature + """ {
              override def copy(""" + copyMethodArgSignatures.mkString(", ") + """) =
                new """ + name + """(""" + copyMethodInstantiationArgs.mkString(", ") + """)
            }
          """
        }

        interpreter.compileString(code)
        val c =
          interpreter.classLoader.findClass(name)
            .asInstanceOf[Class[T with Persisted]]
        interpreter.reset()

        persistedClassCache.update(t, c)

        c
    }

}