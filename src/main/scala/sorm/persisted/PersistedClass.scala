package sorm.persisted

import sorm._
import reflection._
import sext._, embrace._
import com.weiglewilczek.slf4s.Logging

object PersistedClass extends Logging {

  import reflect.runtime.universe._
  import reflect.runtime.{currentMirror => mirror}
  import scala.tools.reflect.ToolBox

  private lazy val toolbox = mirror.mkToolBox()

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

      val newArgNames
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
      toolbox.eval(
        toolbox.parse(
          generateCode(r, generateName())
            .tap{ c => logger.trace("Generating class:\n" + c) }
        )
      ) .asInstanceOf[Class[T with Persisted]]
    }

  private val cache
    = new collection.mutable.HashMap[Reflection, Class[_ <: Persisted]] {
        override def default
          ( k : Reflection )
          = {
            val v = createClass(k)
            update(k, v)
            v
          }
      }
  def apply
    ( r : Reflection )
    = cache(r.mixinBasis)

}