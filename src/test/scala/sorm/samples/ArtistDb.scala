package sorm.samples

import sorm._
import core._
import reflection._
import sext._, embrace._
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
        = "jdbc:mysql://localhost/test",
      initMode
        = InitMode.DropAllCreate
    )
    {
      //  Fill up the db with sample data.
      //  Is placed in the constructor to be executed automatically on initialization of db.
      //  The value names are purposely not hidden anywhere to be accessible for testing purposes
      val cx = connection()
      val ru
        = cx.save( Locale("ru") )
      val en
        = cx.save( Locale("en") )

      val rock
        = cx.save( Style( Map( en -> Seq("Rock"),
                               ru -> Seq("Рок") ) ) )
      val hardRock
        = cx.save( Style( Map( en -> Seq("Hard Rock"),
                               ru -> Seq("Тяжёлый рок", "Тяжелый рок") ) ) )
      val metal
        = cx.save( Style( Map( en -> Seq("Metal"),
                               ru -> Seq("Метал") ) ) )
      val grunge
        = cx.save( Style( Map( en -> Seq("Grunge"),
                               ru -> Seq("Грандж") ) ) )

      val metallica
        = cx.save( Artist( Map( en -> Seq("Metallica"),
                                ru -> Seq("Металика", "Металлика") ),
                           Set( metal, rock, hardRock ) ) )
      val nirvana
        = cx.save( Artist( Map( en -> Seq("Nirvana"),
                                ru -> Seq("Нирвана") ),
                           Set( rock, hardRock, grunge ) ) )
      val kino
        = cx.save( Artist( Map( en -> Seq("Kino"),
                                ru -> Seq("Кино") ),
                           Set( rock ) ) )
      val rollingStones
        = cx.save( Artist( Map( en -> Seq("The Rolling Stones",
                                          "Rolling Stones",
                                          "Rolling Stones, The"),
                                ru -> Seq("Ролинг Стоунз",
                                          "Роллинг Стоунз",
                                          "Роллинг Стоунс",
                                          "Ролинг Стоунс") ),
                           Set( rock ) ) )
      val direStraits
        = cx.save( Artist( Map( en -> Seq("Dire Straits"),
                                ru -> Seq("Даэр Стрэйтс") ),
                           Set( rock ) ) )
      val godsmack
        = cx.save( Artist( Map( en -> Seq("Godsmack"),
                                ru -> Seq("Годсмэк") ),
                           Set( metal, hardRock, rock ) ) )

    }


}
