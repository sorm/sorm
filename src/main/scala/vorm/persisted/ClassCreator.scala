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
        = sourceArgs.map{ _._1 }

      """
      class """ + name + """(""" + newArgSignatures.mkString(", ") + """)
        extends """ + r.signature + """(""" + sourceArgSignatures.mkString(", ") + """)
        with """ + tpe[Persisted].signature + """ {
          override def copy(""" + copyMethodArgSignatures.mkString(", ") + """) =
            new """ + name + """(""" + copyMethodInstantiationArgs.mkString(", ") + """)
        }
      """
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