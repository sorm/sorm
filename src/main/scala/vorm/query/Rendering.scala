package vorm.query

import vorm._
import reflection._
import structure._
import mapping._
import jdbc._
import extensions._

import Query._

object Rendering {

  // implicit class QueryToStatement ( self : Query ) {
  //   def toStatement
  //     = self.toSql.toStatement
  // }

  // implicit class QueryToSql ( self : Query ) {
  //   def toSql
  //     = self
  //         .
  // }

  // implicit class QueryIdSql ( self : Query ) {
  //   def idSql
  //     = self.where.sql
  // }

  implicit class WhereIdSql ( self : Where ) {
    def idSql
      = ???
  }

}