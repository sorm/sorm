package sorm.samples

object ArtistModel {

  case class Artist
    ( names : Seq[Name],
      styles : Set[Style] )

  case class Style
    ( names : Seq[Name] )

  case class Name
    ( locale : Locale,
      value : String )

  case class Locale
    ( code : String )
    
}
