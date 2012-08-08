package vorm.persisted

import tools.nsc.interpreter.IMain
import tools.nsc._

import vorm._
import reflection._
import extensions._

object ClassCreator {

  private lazy val interpreter 
    = {
      val settings = new Settings()
      settings.usejavacp.value = true
      new IMain(settings, new NewLinePrintWriter(new ConsoleWriter, true))
    }

  private var generateNameCounter = 0
  private def generateName() 
    = synchronized {
      generateNameCounter += 1
      "PersistedAnonymous" + generateNameCounter
    }

  def code
    ( r : Reflection,
      name : String )
    = {
      val sourceArgs
        = r.constructorArguments

      val sourceArgSignatures
        = sourceArgs.view
            .map{ case (n, r) => n + " : " + r.signature }
            .toList

      val newArgSignatures
        = "val id : Long" +: sourceArgSignatures

      val copyMethodArgSignatures
        = sourceArgs.map{ case (n, r) => 
            n + " : " + r.signature + " = " + n
          }

      val copyMethodInstantiationArgs
        = "id" +: sourceArgs.map{ _._1 }.toList


      "class " + name + "\n" +
      ( "( " + newArgSignatures.mkString(",\n").indent(2).trim + " )\n" +
        "extends " + r.signature + "( " + 
        sourceArgs.map{_._1}.mkString(", ") + 
        " )\n" +
        "with " + Reflection[Persisted].signature + "\n" + 
        "{\n" +
        ( "override def copy\n" +
          ( "( " + 
            copyMethodArgSignatures.mkString(",\n").indent(2).trim + 
            " )\n" +
            "= " + "new " + name + "( " + 
            copyMethodInstantiationArgs.mkString(", ") + 
            " )\n"
            ).indent(2) 
          ).indent(2) + "\n" +
        "}" )
        .indent(2)
    }

  def createClass
    [ T ]
    ( r : Reflection )
    : Class[T with Persisted]
    = {
      val name = generateName()

      interpreter.compileString(code(r, name))
      val c 
        = interpreter.classLoader.findClass(name)
            .asInstanceOf[Class[T with Persisted]]
      interpreter.reset()

      c
    }

}