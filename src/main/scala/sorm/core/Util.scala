package sorm.core

object Util {
  def toLong ( a : Any ) : Long
    = a match {
        case id : Long => id
        case id : Int => id.toLong
        case id : Short => id.toLong
        case id : Byte => id.toLong
        case id : Number => id.longValue
      }
}
