package sorm.core.expressions

trait Parser[ template <: templates.Action ] {
  type Source
  type Result
  def parse( source: Source ): Result
}
object Parser {

}
