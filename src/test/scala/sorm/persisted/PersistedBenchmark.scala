package sorm.persisted

import sorm._
import reflection.Reflection
import sext.Benchmarking._

object PersistedBenchmark extends App {
  case class A(name: String)
  case class B(title: String, children: List[A])
  case class C()

  val N = 1000

  val c = C()

  PersistedClass(Reflection[C])

  benchmarkAndPrint ("copies") {
    var i = 0
    var l = List[C]()
    while (i < N) {
      l = c.copy() :: l
      i += 1
    }
  }
  benchmarkAndPrint ("persisteds") {
    var i = 0
    var l = List[C]()
    while (i < N) {
      l = Persisted(c, 32) :: l
      i += 1
    }
  }

  def time[A](a: => A) = {
    val now = System.nanoTime
    try {
      a
    } finally {
      val micros = (System.nanoTime - now) / 1000
      println("%d microseconds".format(micros))
    }
  }
}