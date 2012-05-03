package vorm.extensions

object Sandbox extends App {

  assert(342.isInstanceOf1[AnyVal])
  assert(!Seq(123, 2313).isInstanceOf1[AnyVal])
  assert(Seq(123, 2313).isInstanceOf1[Seq[_]])

  assert(Seq(123, 2313).isInstanceOf1[Traversable[_]])
  assert(Map("a" -> 1, "b" -> 0).isInstanceOf1[Traversable[_]])

  assert(Seq(123, 2313).isInstanceOf1[Seq[Int]])
  assert(!Seq(123, 2313).isInstanceOf1[Seq[String]])

  assert(Map("a" -> 1, "b" -> 0).isInstanceOf1[Map[_, _]])
  assert(!Map("a" -> 1, "b" -> 0).isInstanceOf1[AnyVal])

}
