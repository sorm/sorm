package vorm.samples

import vorm._
import api._
import extensions._

object SampleDb {
  
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
      mode
        = Mode.DropAllCreate
    )
    {
      //  Fill up the db with sample data.
      //  Is placed in the constructor to be executed automatically on initialization of db.
      //  Is wrapped in a scope to not dispose the local variables to the Db api.
      {
        val ru
          = Db.save( Locale("ru") )
        val en
          = Db.save( Locale("en") )

        val rock
          = Db.save( Style( Map(en -> Seq("Rock"),
                                ru -> Seq("Рок")) ) )
        val hardRock
          = Db.save( Style( Map(en -> Seq("Hard Rock"),
                                ru -> Seq("Тяжёлый рок", "Тяжелый рок")) ) )
        val metal
          = Db.save( Style( Map(en -> Seq("Metal"),
                                ru -> Seq("Метал")) ) )
        val grunge
          = Db.save( Style( Map(en -> Seq("Grunge"),
                                ru -> Seq("Грандж")) ) )

        Db.save(
          Artist(
            names
              = Map( en -> Seq("Metallica"),
                     ru -> Seq("Металика", "Металлика") ),
            styles
              = Set( metal, rock, hardRock )
          )
        )
        Db.save(
          Artist(
            names
              = Map( en -> Seq("Nirvana"),
                     ru -> Seq("Нирвана") ),
            styles
              = Set( rock, hardRock, grunge )
          )
        )
        Db.save(
          Artist(
            names
              = Map( en -> Seq("Kino"),
                     ru -> Seq("Кино") ),
            styles
              = Set( rock )
          )
        )
        Db.save(
          Artist(
            names
              = Map( en -> Seq("Rolling Stones"),
                     ru -> Seq("Ролинг Стоунз",
                               "Роллинг Стоунз",
                               "Роллинг Стоунс",
                               "Ролинг Стоунс") ),
            styles
              = Set( rock )
          )
        )
        Db.save(
          Artist(
            names
              = Map( en -> Seq("Dire Straits"),
                     ru -> Seq("Даэр Стрэйтс") ),
            styles
              = Set( rock )
          )
        )
        Db.save( Artist( Map( en -> Seq("Godsmack"),
                              ru -> Seq("Годсмэк") ),
                         Set( metal, hardRock, rock ) ) )
      }
    }



}
