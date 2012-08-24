package sorm.extensions


class MapExtensions[K, V](x: Map[K, V]) {
  def filterValues(predicate: V => Boolean) =
    x.filter(pair => predicate(pair._2))
  def withValuesFilter(predicate: V => Boolean) =
    x.withFilter(pair => predicate(pair._2))
  def mapKeys[K2](f: K => K2) =
    x.map(pair => f(pair._1) -> pair._2)

}