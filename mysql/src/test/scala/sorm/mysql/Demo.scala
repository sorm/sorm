package sorm.mysql

import sorm.core.api.where._
import sorm.core.api.select._
import sorm.core.subRef._

object Demo extends App {

  case class Artist( name : String, genres : Set[ Genre ] )
  case class Genre( name : String )

  val db = new MysqlInstance

  val instructions = 
    db.select[ Artist ]
      .where( 
        _.exists( _.genres )(
          _.equals( _.name, "lkdsjf" )
        )
      )

  println( instructions )

}
