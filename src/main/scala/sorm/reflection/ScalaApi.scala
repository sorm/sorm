package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._

object ScalaApi {

  implicit class TypeApi ( t : Type ) {
    def s : Symbol = t.typeSymbol
    private def members : Stream[Symbol] = t.members.toStream
    def properties
      = members.filter{_.isTerm}.filter{!_.isMethod}
    def constructors
      = members.view
          .collect{
            case m : MethodSymbol
              if m.isConstructor && m.owner == s
              => m
          }
          .reverse
    def ancestors
      = s.unfold{ s =>
          if( s.owner == NoSymbol || s.decodedName == "<root>" ) None
          else Some(s -> s.owner)
        }
    def fullName
      = ancestors.foldRight(""){ (s, text) =>
          if( text == "" ) s.decodedName
          else if( s.owner.kind == "class" ) text + "#" + s.decodedName
          else text + "." + s.decodedName
        }

    def instantiate
      ( params : Map[String, Any] )
      : Any
      = constructors
          .view
          .zipBy{ _.params.view.flatten.map{_.decodedName} }
          .find{ _._2.toSet == params.keySet }
          .map{ case (c, ps) => instantiate( ps.map{params}, c ) }
          .get
    def instantiate
      ( args : Traversable[Any] = Nil,
        constructor : MethodSymbol = constructors.head )
      : Any
      = mirror.reflectClass(s.asInstanceOf[ClassSymbol])
          .reflectConstructor(constructor)(args.toSeq : _*)
    def javaClass = mirror.runtimeClass(t)
  }
  implicit class SymbolApi ( s : Symbol ) {
    def decodedName = s.name.toString.trim
  }



}
