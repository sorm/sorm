#SORM. A complete abstraction ORM framework for Scala
SORM is an object-relational mapping framework having simplicity of use and absolute abstraction from relational side at its core principles. It automagically creates database tables, emits queries, inserts, updates and deletes records. This all functionality is presented to the user with a simple API which works on standard Scala's case classes. 

Using SORM there is absolutely no need for the user to know anything about SQL, DDL and what concepts like many-to-many or one-to-many even mean. One can develop an application knowing that database integration will be a trivial step.

##Supported databases
Currently SORM releases are getting tested against MySQL and H2 databases. Other DBs may also be supported but are yet not guaranteed to be.

##Future plans
* Support for other popular DBs

##Getting started
Let's add a dependency to SORM artifact. In Maven it will look like so: 

    <dependency>
      <groupId>com.github.nikita-volkov</groupId>
      <artifactId>sorm</artifactId>
      <version>0.1.0</version>
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
  
    import sorm.Sorm._

    val db
      = new Instance(
          entities
            = Set() +
              Entity[Artist]() +
              Entity[Style]() +
              Entity[Locale](),
          url
            = "jdbc:h2:mem:test"
        )

> If you need an explanation with the code above, we create a SORM instance ready to work with objects of types `Artist`, `Style` and `Locale`. That instance connects to an in-memory H2 database without specifying user or password.

Guess what, that's it! We now have an up and running database connection with a full schema structure required for storing our objects already created for us. All that's left to do is put it to use. 

###Let's populate it with some data:

    //  create locales:
    val ru
      = db.save( Locale("ru") )
    val en
      = db.save( Locale("en") )

    //  create styles:
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

    //  create artists:
    db.save( Artist( Map( en -> Seq("Metallica"),
                          ru -> Seq("Металика", "Металлика") ),
                     Set( metal, rock, hardRock ) ) )
    db.save( Artist( Map( en -> Seq("Nirvana"),
                          ru -> Seq("Нирвана") ),
                     Set( rock, hardRock, grunge ) ) )
    db.save( Artist( Map( en -> Seq("Kino"),
                          ru -> Seq("Кино") ),
                     Set( rock ) ) )
    db.save( Artist( Map( en -> Seq("The Rolling Stones",
                                    "Rolling Stones",
                                    "Rolling Stones, The"),
                          ru -> Seq("Ролинг Стоунз",
                                    "Роллинг Стоунз",
                                    "Роллинг Стоунс",
                                    "Ролинг Стоунс") ),
                     Set( rock ) ) )
    db.save( Artist( Map( en -> Seq("Dire Straits"),
                          ru -> Seq("Даэр Стрэйтс") ),
                     Set( rock ) ) )
    db.save( Artist( Map( en -> Seq("Godsmack"),
                          ru -> Seq("Годсмэк") ),
                     Set( metal, hardRock, rock ) ) )

###Now let's fetch some data from our populated database:

    //  get an artist by id:
    db.one[Artist].filterEquals("id", 2).fetch() // will return Nirvana

    //  all artists having a style that has `Hard Rock` in a list of its names
    db.all[Artist].filterEquals("names.value.item.value", "Hard Rock").fetch()

