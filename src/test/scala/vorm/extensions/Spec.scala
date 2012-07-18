package vorm.extensions

import org.specs2.mutable._

class Spec extends Specification{

  "An Int" should {
    "be an instance of AnyVal" in
      342.isInstanceOf1[AnyVal]
    "be an instance of Int" in
      342.isInstanceOf1[Int]
  }

  "A sequence" should {
    "not be an instance of AnyVal" in
      !Seq(123, 2313).isInstanceOf1[AnyVal]
    "be an instance of Seq" in
      Seq(123, 2313).isInstanceOf1[Seq[_]]
    "be an instance of Seq with same Generic" in
      Seq(123, 2313).isInstanceOf1[Seq[Int]]
    "not be an instance of Seq with different Generic" in
      !Seq(123, 2313).isInstanceOf1[Seq[String]]
    "be an instance of Traversable with same Generic" in 
      Seq(123, 2313).isInstanceOf1[Traversable[Int]]
    "be an instance of Traversable with different Generic" in 
      !Seq(123, 2313).isInstanceOf1[Traversable[String]]

  }
 "A map" should {
   "be an instance of Traversable" in
      Map("a" -> 1, "b" -> 0).isInstanceOf1[Traversable[_]]
   "be an instance of Map" in
      Map("a" -> 1, "b" -> 0).isInstanceOf1[Map[_, _]]
   "not be an instance of AnyVal" in
      !Map("a" -> 1, "b" -> 0).isInstanceOf1[AnyVal]
 }

}
