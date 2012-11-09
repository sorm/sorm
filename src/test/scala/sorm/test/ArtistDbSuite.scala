package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sext._, embrace._

object ArtistDbSuite {
  
  case class Artist
    ( names : Map[Locale, Seq[String]],
      styles : Set[Style] )

  case class Style
    ( names : Map[Locale, Seq[String]] )

  case class Locale
    ( code : String )

}
@RunWith(classOf[JUnitRunner])
class ArtistDbSuite extends FunSuite with ShouldMatchers {

  import ArtistDbSuite._

  TestingInstances.instances(Set() + Entity[Artist]() + Entity[Style]() + Entity[Locale]()) foreach { case (db, dbId) =>
    val ru 
      = db.save( Locale("ru") )
    val en
      = db.save( Locale("en") )

    val rock
      = db.save( Style( Map( en -> Seq("Rock"),
                             ru -> Seq("Рок") ) ) )
    val hardRock
      = db.save( Style( Map( en -> Seq("Hard Rock"),
                             ru -> Seq("Тяжёлый рок", "Тяжелый рок") ) ) )
    val metal
      = db.save( Style( Map( en -> Seq("Metal"),
                             ru -> Seq("Метал") ) ) )
    val grunge
      = db.save( Style( Map( en -> Seq("Grunge"),
                             ru -> Seq("Грандж") ) ) )

    val metallica
      = db.save( Artist( Map( en -> Seq("Metallica"),
                              ru -> Seq("Металика", "Металлика") ),
                         Set( metal, rock, hardRock ) ) )
    val nirvana
      = db.save( Artist( Map( en -> Seq("Nirvana"),
                              ru -> Seq("Нирвана") ),
                         Set( rock, hardRock, grunge ) ) )
    val kino
      = db.save( Artist( Map( en -> Seq("Kino"),
                              ru -> Seq("Кино") ),
                         Set( rock ) ) )
    val rollingStones
      = db.save( Artist( Map( en -> Seq("The Rolling Stones",
                                        "Rolling Stones",
                                        "Rolling Stones, The"),
                              ru -> Seq("Ролинг Стоунз",
                                        "Роллинг Стоунз",
                                        "Роллинг Стоунс",
                                        "Ролинг Стоунс") ),
                         Set( rock ) ) )
    val direStraits
      = db.save( Artist( Map( en -> Seq("Dire Straits"),
                              ru -> Seq("Даэр Стрэйтс") ),
                         Set( rock ) ) )
    val godsmack
      = db.save( Artist( Map( en -> Seq("Godsmack"),
                              ru -> Seq("Годсмэк") ),
                         Set( metal, hardRock, rock ) ) )


    test(dbId + " - path with index"){
      db.access[Artist]
        .whereEqual("names.value(1)", "Rolling Stones")
        .fetchOne()
        .get
        .names(en)(1) should be === "Rolling Stones"
    }
    test(dbId + " - Offset"){
      pending
    }
    test(dbId + " - Limit"){
      pending
    }
    test(dbId + " - Ordering"){
      db.access[Artist].order("id", true).fetch().map(_.id) should equal (6::5::4::3::2::1::Nil)
    }
    test(dbId + " - Contains"){
      pending
    }
    test(dbId + " - Equality to unpersisted entity"){
      pending
    }
    test(dbId + " - Equality to persisted entity"){
      db.access[Artist]
        .whereEqual("styles.item", metal)
        .fetch()
        .flatMap{_.names.values.head}
        .toSet should be === Set("Metallica", "Godsmack")
    }
    test(dbId + " - Map, Set, Seq deep path"){
      db.access[Artist]
        .whereEqual("styles.item.names.value.item", "Hard Rock")
        .fetch()
        .flatMap{_.names.values.head}
        .toSet should be === Set("Metallica", "Nirvana", "Godsmack")
    }
    test(dbId + " - Results have correct id property"){
      db.access[Artist].fetchOne().map{_.id} should be === Some(1)
    }
    test(dbId + " - Query by id"){
      db.access[Artist].whereEqual("id", 1).fetchOne()
        .map{_.names.values.head.head}.get should be === "Metallica"
      db.access[Artist].whereEqual("id", 3).fetchOne()
        .map{_.names.values.head.head}.get should be === "Kino"
    }
  }
}
