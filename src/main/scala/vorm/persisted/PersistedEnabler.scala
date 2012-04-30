package vorm.persisted

import someOtherPackage.domain.Artist
import tools.nsc.interpreter.IMain
import tools.nsc._
import reflect.mirror.{Symbol, Type}

object PersistedEnabler {
  private val persistedClassCache =
    collection.mutable.Map[TypeTag[_], Class[_]]()

  def persistedClass[T](tag: TypeTag[T]): Class[T with Persisted] = {
    if (persistedClassCache.contains(tag))
      persistedClass(tag).asInstanceOf[Class[T with Persisted]]
    else {
      val name = generateName()

      val code = {
        val sourceParams =
          methodParams(constructors(tag.tpe).head.typeSignature)

        val newParamsList = {
          def paramDeclaration(s: Symbol): String =
            s.name.decoded + ": " + s.typeSignature.toString
          "val key: String" :: sourceParams.map(paramDeclaration) mkString ", "
        }
        val sourceParamsList =
          sourceParams.map(_.name.decoded).mkString(", ")

        """
       class """ + name + """(""" + newParamsList + """)
        extends """ + tag.sym.fullName + """(""" + sourceParamsList + """)
        with """ + typeTag[Persisted].sym.fullName + """
      """
      }

      println(code)


      interpreter.compileString(code)
      val c =
        interpreter.classLoader.findClass(name)
          .asInstanceOf[Class[T with Persisted]]

      interpreter.reset()

      persistedClassCache(tag) = c

      c
    }
  }

  def toPersisted[T](instance: T, key: String)
      (implicit instanceTag: TypeTag[T]): T with Persisted = {



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

  type MethodType = {def params: List[Symbol]; def resultType: Type}

  def methodParams(t: Type): List[Symbol] =
    t.asInstanceOf[MethodType].params

  def methodResultType(t: Type): Type =
    t.asInstanceOf[MethodType].resultType

  def constructors(t: Type): Iterable[Symbol] =
    t.members.filter(_.kind == "constructor")

  def fullyQualifiedName(s: Symbol): String = {
    def symbolsTree(s: Symbol): List[Symbol] =
      if (s.enclosingTopLevelClass != s)
        s :: symbolsTree(s.enclosingTopLevelClass)
      else if (s.enclosingPackageClass != s)
        s :: symbolsTree(s.enclosingPackageClass)
      else
        Nil

    symbolsTree(s)
      .reverseMap(_.name.decoded)
      .drop(1)
      .mkString(".")
  }

}
