package vorm.persisted

import tools.nsc.interpreter.IMain
import tools.nsc._
import reflect.mirror._

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

        val copyMethodParamsList =
          sourceParams.map(s => s.name.decoded + ": " + s.typeSignature.toString + " = " + s.name.decoded).mkString(", ")

        val copyInstantiationParamsList =
          "key" :: sourceParams.map(_.name.decoded) mkString ", "

        """
        class """ + name + """(""" + newParamsList + """)
          extends """ + tag.sym.fullName + """(""" + sourceParamsList + """)
          with """ + typeTag[Persisted].sym.fullName + """ {
//            def copy(""" + copyMethodParamsList + """) =
//              new """ + name + """(""" + copyInstantiationParamsList + """)
          }
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

  def toPersisted[T <: AnyRef](instance: T, key: String)
                              (implicit instanceTag: TypeTag[T]): T with Persisted = {

    val c = persistedClass(instanceTag)


    val args = {
      val valuesMap = propertyValues(instance)
      key ::
        methodParams(constructors(instanceTag.tpe).head.typeSignature)
          .map(_.name.decoded.trim)
          .map(valuesMap(_))

    }

    println(args)

    c.getConstructors.head
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
    "PersistedAnonymous" + generateNameCounter.toString
  }


  def propertyNames(t: Type) =
    t.members.filter(m => !m.isMethod && m.isTerm).map(_.name.decoded.trim)

  def propertyValues[T <: AnyRef](instance: T) = {
    val t = typeOfInstance(instance)

    propertyNames(t)
      .map(n => n -> invoke(instance, t.member(newTermName(n)))()).toMap
  }

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
