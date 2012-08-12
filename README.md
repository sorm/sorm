#WORM. A complete abstraction ORM framework for Scala
WORM is an object-relational mapping framework having simplicity of use and absolute abstraction from relational side at its core principles. It automagically creates database tables, emits queries, inserts, updates and deletes records. This all functionality is presented to the user with a simple API which works with standard Scala's case classes. Using WORM there is absolutely no need for the user to know anything about SQL and DDL and what concepts like many-to-many or one-to-many even mean. Also besides being completely abstra
WORM also provides an 

You can develop your application knowing that database integration will be a trivial step.


#Features -- fuck that
1. Maximum decoupling of the model from database. Your mapped objects don't need to extend any interface

#Getting started
Let's add a dependency to WORM artifact. In Maven it will look like so: 

    <dependency>
      <groupId>com.github.nikita-volkov</groupId>
      <artifactId>worm</artifactId>
      <version>0.1.0</version>
    </dependency>

For our testing project we will use an in-memory version of H2 database - let's add a dependency for it too:

    <dependency>
      <groupId>com.h2database</groupId>
      <artifactId>h2</artifactId>
      <version>1.3.168</version>
    </dependency>

Now to the actual program. Please consider a data model described with the following standard case classes:

    case class Artist
      ( names : Map[Locale, Seq[String]],
        styles : Set[Style] )

    case class Style
      ( names : Map[Locale, Seq[String]] )

    case class Locale
      ( code : String )

> You can see that instead of assigning the `Style` and `Artist` objects with a common `name` property we've decided here to go a bit tougher way to let our application support different locales and to allow it to store different variants of names for the same entity if there exist alternatives. For instance, in the English locale one artist we will store will have three alternative names: "The Rolling Stones", "Rolling Stones" and "Rolling Stones, The".

Now let's initialize our WORM instance:
  
    import worm._

    object Db
      extends Instance(
        entities
          = Seq( Entity[Artist](), 
                 Entity[Style](), 
                 Entity[Locale]() ),
        url
          = "jdbc:h2:mem:test"
      )

> If you need an explanation, with the code above we create a WORM instance ready to work with objects of types `Artist`, `Style` and `Locale`, that instance will connect to the in-memory H2 database without specifying user or password.

Guess what, that's it - we now have an up and running database connection with a full schema structure required for storing our objects already created for us. All that's left to do is to put it to use - let's populate it with some data:

    //  create locales:
    val ru
      = Db.save( Locale("ru") )
    val en
      = Db.save( Locale("en") )

    //  create styles:
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

    //  create artists:
    Db.save( Artist( Map( en -> Seq("Metallica"),
                          ru -> Seq("Металика", "Металлика") ),
                     Set( metal, rock, hardRock ) ) )
    Db.save( Artist( Map( en -> Seq("Nirvana"),
                          ru -> Seq("Нирвана") ),
                     Set( rock, hardRock, grunge ) ) )
    Db.save( Artist( Map( en -> Seq("Kino"),
                          ru -> Seq("Кино") ),
                     Set( rock ) ) )
    Db.save( Artist( Map( en -> Seq("The Rolling Stones",
                                    "Rolling Stones",
                                    "Rolling Stones, The"),
                          ru -> Seq("Ролинг Стоунз",
                                    "Роллинг Стоунз",
                                    "Роллинг Стоунс",
                                    "Ролинг Стоунс") ),
                     Set( rock ) ) )
    Db.save( Artist( Map( en -> Seq("Dire Straits"),
                          ru -> Seq("Даэр Стрэйтс") ),
                     Set( rock ) ) )
    Db.save( Artist( Map( en -> Seq("Godsmack"),
                          ru -> Seq("Годсмэк") ),
                     Set( metal, hardRock, rock ) ) )

Now let's fetch some data from our populated database:

    //  all artists having a style that has `Hard Rock


    //  get an artist by id:
    Db.byId[Artist](2) // will return a Nirvana





#API
