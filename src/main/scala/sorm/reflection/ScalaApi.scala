package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._

object ScalaApi {

  implicit class TypeApi ( val t : Type ) extends AnyVal {
    def s : Symbol = t.typeSymbol
    def members : Stream[Symbol] = t.members.toStream
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
          if( s.owner == NoSymbol || name(s) == "<root>" ) None
          else Some(s -> s.owner)
        }
    def fullName
      = ancestors.foldRight(""){ (s, text) =>
          if( text == "" ) name(s)
          else if( s.owner.kind == "class" ) text + "#" + name(s)
          else text + "." + name(s)
        }
    def instantiate
      ( args : Traversable[Any] = Nil, 
        constructor : MethodSymbol = constructors.head )
      : Any
      = mirror.reflectClass(s.asInstanceOf[ClassSymbol])
          .reflectConstructor(constructor)(args.toSeq : _*)

    def instantiate
      ( params : Map[String, Any] )
      : Any
      = constructors
          .view
          .zipBy{ _.params.view.flatten.map{name} }
          .find{ _._2.toSet == params.keySet }
          .map{ case (c, ps) => instantiate( ps.map{params}, c ) }
          .get

          // .find{
          //   _.params.view.flatten.map{name}.toSet ==
          //   params.keySet
          // }
          // .map{ c => 
          //   instrantiate(
          //     c.params.view.flatten.map{name}.map{params}, 
          //     c
          //   )
          // }
          // .get
      // = instantiate( constructorArguments.view.unzip._1.map{params} )

    def javaClass = mirror.runtimeClass(t)

    def name (s : Symbol) = s.name.toString.trim
    
  }



}
