package sorm.mirrorQuirks

import reflect.mirror._

/**
 * Trickery and black magic around Scala's Mirror API
 */
object MirrorQuirks {

  def isMixedIn(t: Type) =
    t.kind == "RefinedType"

  /**
   * Either the type itself if it's not mixed in or the first of its parents
   */
  def mixinBasis(t: Type) =
    if (isMixedIn(t)) t.parents.head
    else t

  def properties(t: Type) =
    mixinBasis(t) match { case t =>
      t.members
        .filter(m => !m.isMethod && m.owner == t.typeSymbol)
        .toList
    }

  def methods(t: Type) =
    mixinBasis(t) match { case t =>
      t.members
        .filter(m => m.isMethod && m.owner == t.typeSymbol)
        .toList
    }

  def generics(t: Type) =
    mixinBasis(t).typeArguments

  def constructors(t: Type) =
    mixinBasis(t) match { case t =>
      t.members
        .filter(m => m.kind == "constructor" && m.owner == t.typeSymbol)
        .toList.reverse
    }

  //  misses mixin support
  def signature(t: Type): String =
    generics(t) match {
      case Nil =>
        fullName(t.typeSymbol)
      case gs =>
        fullName(t.typeSymbol) + "[" + gs.map(signature).mkString(", ") + "]"
    }

  def javaClass(mt: Type): Class[_] =
    try typeToClass(mt)
    catch {
      case _ : ClassNotFoundException =>
        mt.typeSymbol match {
          case s if s.fullName == "scala.Any" => classOf[Any]
          case s =>
            classByName(javaClassName(s))
        }
      case _ : NoClassDefFoundError =>
        ???
    }

  def classByName(n: String) =
    symbolToClass(symbolForName(n))

  def name(s: Symbol): String =
    s.name.decoded.trim match {
      //  mixed in types bug workaround
      case "<refinement>" =>
        name(s.typeSignature.parents.head.typeSymbol)
      case n => n
    }

  def javaClassName(s: Symbol): String =
    s.owner match {
      case o if o.isPackageClass =>
        s.fullName
      case o if o.isClass =>
        javaClassName(o) + "$" + name(s)
    }

  def symbolsTree(s: Symbol): List[Symbol] =
    s.owner match {
      case NoSymbol => Nil
      case o => s :: symbolsTree(o)
    }

  def isObject(s: Symbol) =
    s.kind == "object"

  def isClass(s: Symbol) =
    s.kind == "class"

  /**
   * Checks whether this symbol is nested inside a class (not a static object)
   */
  def isInner(s: Symbol) =
    isClass(s.owner)

  def fullName(s: Symbol) =
    symbolsTree(s).foldRight("") {
      (s, text) =>
        if (text == "") name(s)
        else if (isInner(s)) text + "#" + name(s)
        else text + "." + name(s)
    }
}
