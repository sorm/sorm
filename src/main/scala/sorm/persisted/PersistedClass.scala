package sorm.persisted

import sorm._
import reflection._

import sext._, embrace._
import com.typesafe.scalalogging.{StrictLogging => Logging}

object PersistedClass extends Logging {

  import reflect.runtime.universe._
  import scala.tools.reflect.ToolBox

  private var generateNameCounter = 0
  private def generateName() 
    = synchronized {
        generateNameCounter += 1
        "PersistedAnonymous" + generateNameCounter
      }

  private[persisted] def generateCode
    ( r : Reflection,
      name : String )
    : String
    = {
      val sourceArgs : List[(String, Reflection)]
        = r.primaryConstructorArguments

      val sourceArgSignatures
        = sourceArgs.view
            .map{ case (n, r) => n + " : " + r.signature }
            .toList

      val newArgSignatures : Seq[String]
        = "val id : Long" +: sourceArgSignatures

      val copyMethodArgSignatures
        = sourceArgs.map{ case (n, r) => 
            n + " : " + r.signature + " = " + n
          }

      val oldArgNames
        = sourceArgs.map{ _._1 }

      val newArgNames
        = "id" +: oldArgNames


      "class " + name + "\n" +
      ( "( " + newArgSignatures.mkString(",\n").indent(2).trim + " )\n" +
        "extends " + r.signature + "( " + 
        sourceArgs.map{_._1}.mkString(", ") + 
        " )\n" +
        "with " + Reflection[Persisted].signature + "\n" + 
        "{\n" +
        (
          "type T = " + r.signature + "\n" +
          "override def mixoutPersisted[ T ]\n" +
          ( "= ( id, new " + r.signature + "(" + oldArgNames.mkString(", ") + ").asInstanceOf[T] )" ).indent(2) + "\n" +
          "override def copy\n" +
          ( "( " + 
            copyMethodArgSignatures.mkString(",\n").indent(2).trim + 
            " )\n" +
            "= " + "new " + name + "( " + 
            newArgNames.mkString(", ") + 
            " )\n"
          ) .indent(2) + "\n" +
          "override def productElement ( n : Int ) : Any\n" +
          ( "= " + 
            ( "n match {\n" +
              ( ( for { (n, i) <- newArgNames.view.zipWithIndex }
                  yield "case " + i + " => " + n
                ) :+ 
                "case _ => throw new IndexOutOfBoundsException(n.toString)"
              ).mkString("\n").indent(2) + "\n" +
              "}"
            ) .indent(2).trim
          ) .indent(2) + "\n" +
          "override def productArity = " + newArgNames.size + "\n" +
          "override def equals ( other : Any )\n" +
          ( "= " +
            ( "other match {\n" +
              ( "case other : " + Reflection[Persisted].signature + " =>\n" + (
                  "id == other.id && super.equals(other)"
                ).indent(2) + "\n" +
                "case _ =>\n" +
                "false".indent(2)
              ).indent(2) + "\n" +
              "}"
            ).indent(2).trim
          ).indent(2)
        ).indent(2) + "\n" +
        "}" )
        .indent(2) + "\n" +
        "classOf[" + name + "]"
    }

  private[persisted] def createClass
    [ T ]
    ( r : Reflection )
    : Class[T with Persisted]
    = {
      val mirror = runtimeMirror(Thread.currentThread().getContextClassLoader)
      val toolbox = mirror.mkToolBox()

      toolbox.eval(
        toolbox.parse(
          generateCode(r, generateName())
            .tap{ c => logger.trace("Generating class:\n" + c) }
        )
      ) .asInstanceOf[Class[T with Persisted]]
    }

  private val classesCache = new {
    private def currentClassLoader = Thread.currentThread().getContextClassLoader
    private var cachedClassLoader = currentClassLoader
    private val map = new collection.mutable.WeakHashMap[Reflection, Class[_ <: Persisted]]
    def resolve(r: Reflection) = synchronized {
      val classLoader = currentClassLoader
      if(classLoader != cachedClassLoader) {
        logger.debug("Classloader changed, discarding PersistedClass cache")
        cachedClassLoader = classLoader
        map.clear()
      }
      map.getOrElseUpdate( r, createClass(r) )
    }
  }

  def apply ( r : Reflection ) = classesCache.resolve(r.mixinBasis)

}
