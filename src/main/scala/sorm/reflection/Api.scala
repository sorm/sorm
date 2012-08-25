package sorm.reflection

import reflect.runtime.universe._
import sorm.extensions.Extensions._

trait Api {

  def t : Type

  def s : Symbol = t.typeSymbol

  private[sorm] def members : Stream[Symbol] = t.members.toStream

  private def name ( s : Symbol ) = s.name.toString

  def properties
    = members.filter{_.isTerm}.filter{!_.isMethod}
        .map{ s => name(s) -> Reflection(s.typeSignature) }
        .toMap

  def generics
    = t match {
        case t : TypeRef => t.args.toStream.map{ Reflection(_) }
      }

  def signature : String
    = if( generics.isEmpty ) fullName
      else fullName + "[" + generics.map(_.signature).mkString(", ") + "]"

  private def symbolsTree
    = s.unfold{ s => if( s.owner == NoSymbol ) None else Some(s -> s.owner) }

  def fullName
    = symbolsTree.foldRight(""){ (s, text) =>
        if( text == "" ) name(s)
        else if( s.owner.kind == "class" ) text + "#" + name(s)
        else text + "." + name(s)
      }

  def name : String = name(s)

  def constructorArguments
    = members.view
        .collect{ case m : MethodSymbol if m.isConstructor && m.owner == s => m }
        .reverse.head
        .params.view.flatten
        .map{ s => name(s) -> Reflection(s.typeSignature) }


  override def toString = t.toString
}
