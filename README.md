#SORM. A case-classes oriented ORM framework for Scala

SORM is an object-relational mapping framework having elegance, consistency and simplicity at its primary principles. It is absolutely abstracted from relational side automagically creating database tables, emitting queries, inserting, updating and deleting records. This all functionality is presented to the user with a simple API around standard Scala's case classes. 

##Supported databases
Currently SORM supports MySQL and H2 databases. Support for other popular DBs is comming.

##Supported Scala versions
2.10.0-RC1 and later

##Getting started
Let's add a dependency to SORM artifact. In Maven it will look like so: 

    <dependency>
      <groupId>com.github.nikita-volkov</groupId>
      <artifactId>sorm</artifactId>
      <version>0.2.0</version>
    </dependency>

For our testing project we will use an in-memory version of H2 database - let's add a dependency for it too:

    <dependency>
      <groupId>com.h2database</groupId>
      <artifactId>h2</artifactId>
      <version>1.3.168</version>
    </dependency>

###Now, to the actual program. 
Please consider a data model described with these standard case classes:

    case class Artist
      ( names : Map[Locale, Seq[String]],
        styles : Set[Style] )

    case class Style
      ( names : Map[Locale, Seq[String]] )

    case class Locale
      ( code : String )

> You can see that instead of assigning the `Style` and `Artist` objects with a common `name` property we've decided here to go a bit tougher way to let our application support different locales and to allow it to store different variants of names for the same entity if there exist alternatives. For instance, in the English locale one artist we will store will have three alternative names: "The Rolling Stones", "Rolling Stones" and "Rolling Stones, The".

###Let's initialize our SORM instance:
  
    import sorm._

    val db
      = new Instance(
          entities
            = Set() +
              Entity[Artist]() +
              Entity[Style]() +
              Entity[Locale](),
          url
            = "jdbc:h2:mem:test",
          user
            = "",
          password
            = ""
        )

> If you need an explanation with the code above, we create a SORM instance ready to work with objects of types `Artist`, `Style` and `Locale`. That instance connects to an in-memory H2 database without specifying user or password.

Guess what, that's it! We now have an up and running database with a full schema structure required for storing our objects already created for us. All that's left to do is put it to use. 

###Let's open a connection to that db:

    val cx = db.connection()

###Let's populate it with some data:

    //  create locales:
    val ru = cx.save( Locale("ru") )
    val en = cx.save( Locale("en") )

    //  create styles:
    val rock      = cx.save( Style( Map( en -> Seq("Rock"),
                                         ru -> Seq("Рок") ) ) )
    val hardRock  = cx.save( Style( Map( en -> Seq("Hard Rock"),
                                         ru -> Seq("Тяжёлый рок", 
                                                   "Тяжелый рок") ) ) )
    val metal     = cx.save( Style( Map( en -> Seq("Metal"),
                                         ru -> Seq("Метал") ) ) )
    val grunge    = cx.save( Style( Map( en -> Seq("Grunge"),
                                         ru -> Seq("Грандж") ) ) )

    //  create artists:
    cx.save( Artist( Map( en -> Seq("Metallica"),
                          ru -> Seq("Металика", "Металлика") ),
                     Set( metal, rock, hardRock ) ) )
    cx.save( Artist( Map( en -> Seq("Nirvana"),
                          ru -> Seq("Нирвана") ),
                     Set( rock, hardRock, grunge ) ) )
    cx.save( Artist( Map( en -> Seq("Kino"),
                          ru -> Seq("Кино") ),
                     Set( rock ) ) )
    cx.save( Artist( Map( en -> Seq("The Rolling Stones",
                                    "Rolling Stones",
                                    "Rolling Stones, The"),
                          ru -> Seq("Ролинг Стоунз",
                                    "Роллинг Стоунз",
                                    "Роллинг Стоунс",
                                    "Ролинг Стоунс") ),
                     Set( rock ) ) )
    cx.save( Artist( Map( en -> Seq("Dire Straits"),
                          ru -> Seq("Даэр Стрэйтс") ),
                     Set( rock ) ) )
    cx.save( Artist( Map( en -> Seq("Godsmack"),
                          ru -> Seq("Годсмэк") ),
                     Set( metal, hardRock, rock ) ) )

###Now let's fetch some data from our populated database:

    //  get an artist by id:
    val nirvana
      = cx.access[Artist].whereEquals("id", 2).fetchOne() // will return Nirvana

    //  all artists having a style that has `Hard Rock` in a list of its names
    val hardRockArtists
      = cx.access[Artist].whereEquals("names.value.item.value", "Hard Rock").fetch()

##Diving deeper

For more info please visit the [wiki](https://github.com/nikita-volkov/sorm/wiki) or learn the [API](http://nikita-volkov.github.com/sorm/api/) (you're really interested in the contents of a plain `sorm._` package only).

##Support

Support will be provided at [StackOverflow](http://stackoverflow.com/). Go ahead and ask your questions under tag "sorm".

##Bug-reporting and Tickets

Please post any issues you come across in the [issues](https://github.com/nikita-volkov/sorm/issues) section.

##Contribution
    
It is a very large project, and any kind of contribution is much appreciated. So if you find anything that you think Sorm could evolve on, go ahead and fork and leave your pull requests. Currently, the most wanted updates are drivers for other DBRMs.
