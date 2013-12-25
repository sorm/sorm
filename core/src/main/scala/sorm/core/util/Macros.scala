package sorm.core.util

import scala.reflect.macros.Context

class Macros[c <: Context](val c: c) {

  import c.universe._

  def selectTerm(string: String): Tree =
    string.split('.').view.map(s => newTermName(s)) match {
      case head +: tail => tail.foldLeft(Ident(head): Tree)((a, b) => Select(a, b))
    }
  def selectType(string: String): Tree =
    string.split('.').view match {
      case pathParts :+ name => pathParts.map(newTermName) match {
        case head +: tail => {
          val path = tail.foldLeft(Ident(head): Tree)((a, b) => Select(a, b))
          Select(path, newTypeName(name))
        }
        case _ => Ident(newTypeName(name))
      }
    }
}
