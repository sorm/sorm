package vorm.persisted

trait Persisted[T] {
  def pk: T
}
