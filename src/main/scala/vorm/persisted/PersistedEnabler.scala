package vorm.persisted

import tools.nsc.interpreter.IMain
import tools.nsc._
import reflect.mirror.Symbol

object PersistedEnabler {

  def toPersisted[T](instance: T, key: String)
      (implicit instanceType: TypeTag[T]): T with Persisted = {

    val theClass = {
      val name = generateName()


      val code =
        """
        import """ + fullyQualifiedName(instanceType) + """
        import """ + fullyQualifiedName(tag[Persisted]) + """

        class """ + name + """
          extends """ + instanceType.sym.name + """
          with Persisted {
            val key = """" + key.replace("\"", "\\\"") + """"
          }

        """

      println(code)


      interpreter.compileString(code)
      val c =
        interpreter.classLoader.findClass(name)
          .asInstanceOf[Class[T with Persisted]]

      interpreter.reset()
      c
    }

    throw new NotImplementedError
  }

  private lazy val interpreter = {
    val settings = new Settings()
    settings.usejavacp.value = true
    new IMain(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  }


  private var generateNameCounter = 0l
  private def generateName() = synchronized {
    generateNameCounter += 1
    "PersistedAnonymous" + generateNameCounter.toString
  }
  //
  //
  //  def buildClass[T, V](implicit t: Manifest[T], v: Manifest[V]) = {
  //
  //    // Create a unique ID
  //    val name = generateName()
  //
  //    // what's the Scala code we need to generate this class?
  //    val classDef = """
  //      class %s extends %s with %s
  //    """.format(name, t.toString, v.toString)
  //
  //    println(classDef)
  //
  //    // fire up a new Scala interpreter/compiler
  //    val interpreter = new Interpreter()
  //
  //    // define this class
  //    interpreter.compileString(classDef)
  //
  //    // get the bytecode for this new class
  //    val bytes = interpreter.classLoader.classBytes(name)
  //
  //    // define the bytecode using this classloader; cast it to what we expect
  //    defineClass(name, bytes, 0, bytes.length).asInstanceOf[Class[T with V]]
  //  }


  def fullyQualifiedName(t: TypeTag[_]): String = {
    def symbolsTree(s: Symbol): List[Symbol] =
      if (s.enclosingTopLevelClass != s)
        s :: symbolsTree(s.enclosingTopLevelClass)
      else if (s.enclosingPackageClass != s)
        s :: symbolsTree(s.enclosingPackageClass)
      else
        Nil

    symbolsTree(t.sym)
      .reverseMap(_.name.decoded)
      .drop(1)
      .mkString(".")
  }

}
