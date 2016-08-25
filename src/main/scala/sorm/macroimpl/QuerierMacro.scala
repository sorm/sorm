package sorm.macroimpl

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import sorm.Querier
/**
  * Created by takezoux2 on 2016/08/23.
  */
object QuerierMacro {
  def whereEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereEqual", p, v)
  }
  def whereNotEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereNotEqual", p, v)
  }
  def whereLargerImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereLarger", p, v)
  }
  def whereLargerOrEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereLargerOrEqual", p, v)
  }
  def whereSmallerImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereSmaller", p, v)
  }
  def whereSmallerOrEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereSmallerOrEqual", p, v)
  }
  def whereLikeImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereLike", p, v)
  }
  def whereNotLikeImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    whereImpl(c)("whereNotLike", p, v)
  }
  def whereInImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[Seq[A]]) : c.Expr[Querier[T]] = {
    whereSeqImpl(c)("whereIn", p, v)
  }
  def whereNotInImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[Seq[A]]) : c.Expr[Querier[T]] = {
    whereSeqImpl(c)("whereNotIn", p, v)
  }

  def whereRegexImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    whereImpl[T,String](c)("whereRegex", p, v)
  }

  def whereNotRegexImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    whereImpl[T,String](c)("whereNotRegex", p, v)
  }

  def whereImpl[T <: AnyRef,A](c: Context)(methodName: String, p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    _whereImpl[T,A](c)(methodName,p,v,v.actualType)
  }

  def whereSeqImpl[T <: AnyRef,A](c: Context)(methodName: String, p: c.Expr[T => A], v: c.Expr[Seq[A]]) : c.Expr[Querier[T]] = {
    _whereImpl[T,A](c)(methodName,p,v,v.actualType.typeArgs(0))
  }


  def _whereImpl[T <: AnyRef,A](c: Context)(methodName: String, p: c.Expr[T => A], v: c.Expr[Any], passedType: c.Type) : c.Expr[Querier[T]] = {
    import c.universe._

    // format check
    val fieldName = p.tree match{
      case Function(args,Select(ident,TermName(fieldName))) => {
        fieldName
      }
      case _ => c.abort(c.enclosingPosition,"Field must be simple expression as such .whereEqual(_.userId, 2)")
    }

    // type check
    val fieldType = p.actualType.typeArgs(1)
    if(!(passedType <:< fieldType)){
      // check convertable primitive type
      if( ((passedType =:= typeOf[Int]) && (fieldType =:= typeOf[Long])) ||
          ((passedType =:= typeOf[Float]) && (fieldType =:= typeOf[Double]))){
        // OK
      } else {
        c.abort(c.enclosingPosition, s"Field:${fieldName} is ${fieldType} but passed ${passedType}")
      }
    }

    //println(showRaw(field))
    val tree = q"""${c.prefix}.${TermName(methodName)}(${fieldName},${v})"""
    c.Expr[Querier[T]](tree)
  }

  def orderByImpl[T <: AnyRef](c: Context)( p: c.Expr[T => Any], reverse: c.Expr[Boolean]) : c.Expr[Querier[T]] = {
    import c.universe._

    //println(showRaw(field))
    val tree = p.tree match{
      case Function(args,Select(ident,TermName(field))) => {
        q"""${c.prefix}.order(${field},${reverse})"""
      }
      case _ => c.abort(c.enclosingPosition,"Field must be simple expression as such .whereEqual(_.userId,2)")
    }
    c.Expr[Querier[T]](tree)
  }

}
