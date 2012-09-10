package sorm.samples

import sorm._
import core._
import reflection._
import sext.Sext._
import Sorm._

object ArtistDb {
  
  // A model:

  case class Artist
    ( names : Map[Locale, Seq[String]],
      styles : Set[Style] )

  case class Style
    ( names : Map[Locale, Seq[String]] )

  case class Locale
    ( code : String )


  //  A db instance:

  object Db
    extends Instance(
      entities
        = Seq( Entity[Artist](), 
               Entity[Style](), 
               Entity[Locale]() ),
      url
        = "jdbc:h2:mem:test",
      initMode
        = InitMode.DropAllCreate
    )
    {
      //  Fill up the db with sample data.
      //  Is placed in the constructor to be executed automatically on initialization of db.
      //  The value names are purposely not hidden anywhere to be accessible for testing purposes
      val ru
        = Db.save( Locale("ru") )
      val en
        = Db.save( Locale("en") )

      val rock
        = Db.save( Style( Map( en -> Seq("Rock"),
                               ru -> Seq("Рок") ) ) )
      val hardRock
        = Db.save( Style( Map( en -> Seq("Hard Rock"),
                               ru -> Seq("Тяжёлый рок", "Тяжелый рок") ) ) )
      val metal
        = Db.save( Style( Map( en -> Seq("Metal"),
                               ru -> Seq("Метал") ) ) )
      val grunge
        = Db.save( Style( Map( en -> Seq("Grunge"),
                               ru -> Seq("Грандж") ) ) )

      val metallica
        = Db.save( Artist( Map( en -> Seq("Metallica"),
                                ru -> Seq("Металика", "Металлика") ),
                           Set( metal, rock, hardRock ) ) )
      val nirvana
        = Db.save( Artist( Map( en -> Seq("Nirvana"),
                                ru -> Seq("Нирвана") ),
                           Set( rock, hardRock, grunge ) ) )
      val kino
        = Db.save( Artist( Map( en -> Seq("Kino"),
                                ru -> Seq("Кино") ),
                           Set( rock ) ) )
      val rollingStones
        = Db.save( Artist( Map( en -> Seq("The Rolling Stones",
                                          "Rolling Stones",
                                          "Rolling Stones, The"),
                                ru -> Seq("Ролинг Стоунз",
                                          "Роллинг Стоунз",
                                          "Роллинг Стоунс",
                                          "Ролинг Стоунс") ),
                           Set( rock ) ) )
      val direStraits
        = Db.save( Artist( Map( en -> Seq("Dire Straits"),
                                ru -> Seq("Даэр Стрэйтс") ),
                           Set( rock ) ) )
      val godsmack
        = Db.save( Artist( Map( en -> Seq("Godsmack"),
                                ru -> Seq("Годсмэк") ),
                           Set( metal, hardRock, rock ) ) )

    }


}
