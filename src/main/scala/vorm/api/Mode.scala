package vorm.api

sealed trait Mode
object Mode {
  case object DropAllCreate extends Mode
  case object DropCreate extends Mode
  case object Create extends Mode
  case object None extends Mode
}