package sorm.macroimpl

import sorm.Querier

import scala.language.experimental.macros
import scala.reflect.macros.Context
/**
  * Created by takezoux2 on 2016/08/23.
  */
object QuerierMacro {

  def throwsError = {
    throw new Exception("Sorry, type safe query is not supported in scala 2.10")
  }

  def whereEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereNotEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereLargerImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereLargerOrEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereSmallerImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereSmallerOrEqualImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereLikeImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereNotLikeImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereInImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[Iterable[A]]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def whereNotInImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => A], v: c.Expr[Iterable[A]]) : c.Expr[Querier[T]] = {
    throwsError
  }

  def whereRegexImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    throwsError
  }

  def whereNotRegexImpl[T <: AnyRef](c: Context)( p: c.Expr[T => String], v: c.Expr[String]) : c.Expr[Querier[T]] = {
    throwsError
  }

  def whereContainsImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => Iterable[A]], v: c.Expr[A]) : c.Expr[Querier[T]] ={
    throwsError
  }

  def whereNotContainsImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => Iterable[A]], v: c.Expr[A]) : c.Expr[Querier[T]] ={
    throwsError
  }

  def whereIncludesImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => Iterable[A]], v: c.Expr[Iterable[A]]) : c.Expr[Querier[T]] ={
    throwsError
  }

  def whereNotIncludesImpl[T <: AnyRef,A](c: Context)( p: c.Expr[T => Iterable[A]], v: c.Expr[Iterable[A]]) : c.Expr[Querier[T]] ={
    throwsError
  }

  def whereImpl[T <: AnyRef,A](c: Context)(methodName: String, p: c.Expr[T => A], v: c.Expr[A]) : c.Expr[Querier[T]] = {
    throwsError
  }

  def whereIterableImpl[T <: AnyRef,A](c: Context)(methodName: String, p: c.Expr[T => A], v: c.Expr[Iterable[A]]) : c.Expr[Querier[T]] = {
    throwsError
  }
  def orderByImpl[T <: AnyRef](c: Context)( p: c.Expr[T => Any], reverse: c.Expr[Boolean]) : c.Expr[Querier[T]] = {
    throwsError
  }

}
