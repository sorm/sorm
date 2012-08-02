package vorm.samples

object ArtistModelWithIds {

  case class Artist
    ( names : Seq[Name],
      styles : Set[Style],
      id : Option[Long] = None )

  case class Style
    ( names : Seq[Name],
      id : Option[Long] = None )

  case class Name
    ( locale : Locale,
      value : String,
      id : Option[Long] = None )

  case class Locale
    ( code : String,
      id : Option[Long] = None )

}
