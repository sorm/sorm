package vorm.api

sealed trait InitializationMode
object InitializationMode {
  case object DropCreate extends InitializationMode
}