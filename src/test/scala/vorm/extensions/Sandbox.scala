package vorm.extensions

object Sandbox extends App {

  println {
    0 -> List(2,3,4) foldLeft (_ + _)
  }


}
