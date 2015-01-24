package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sext._, embrace._

object ScalaApi {

  implicit class TypeApi ( t : Type ) {
    def s : Symbol = t.typeSymbol
    private def members : Stream[Symbol] = t.members.toStream
    def properties
      = members.filter(_.isTerm).map(_.asTerm).filter(_.isAccessor)
    def constructors
      = members.view
          .collect{
            case m : MethodSymbol
              if m.isConstructor && m.owner == s
              => m
          }
          .reverse

    def javaClass = mirror.runtimeClass(t)
  }
  implicit class SymbolApi ( s : Symbol ) {
    def t : Type = s.typeSignature
    def decodedName = s.name.toString.trim
    def instantiate
      ( constructor : MethodSymbol,
        args : Traversable[Any] = Nil )
      : Any
      = s match {
          case s : ClassSymbol =>
            mirror.reflectClass(s)
              .reflectConstructor(constructor)(args.toSeq : _*)
        }
    def ancestors
      = s.unfold{ s =>
          if( s.owner == NoSymbol || s.decodedName == "<root>" ) None
          else Some(s -> s.owner)
        }
  }

}
