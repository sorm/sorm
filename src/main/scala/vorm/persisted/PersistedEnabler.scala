package vorm.persisted

import tools.nsc.Interpreter

object PersistedEnabler {

  object DynamicClassLoader {
    private var id = 0l
    def uniqueId = synchronized {id += 1; "Klass" + id.toString}
  }

  class DynamicClassLoader extends java.lang.ClassLoader(getClass.getClassLoader) {
    def buildClass[T, V](implicit t: Manifest[T], v: Manifest[V]) = {

      // Create a unique ID
      val id = DynamicClassLoader.uniqueId

      // what's the Scala code we need to generate this class?
      val classDef = "class %s extends %s with %s".
        format(id, t.toString, v.toString)

      println(classDef)

      // fire up a new Scala interpreter/compiler
      val interpreter = new Interpreter()

      // define this class
      interpreter.compileString(classDef)

      // get the bytecode for this new class
      val bytes = interpreter.classLoader.classBytes(id)

      // define the bytecode using this classloader; cast it to what we expect
      defineClass(id, bytes, 0, bytes.length).asInstanceOf[Class[T with V]]
    }

  }
}
