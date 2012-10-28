#SORM. A case-classes oriented ORM framework for Scala

SORM is an object-relational mapping framework having elegance, consistency and simplicity at its primary principles. It is absolutely abstracted from relational side automagically creating database tables, emitting queries, inserting, updating and deleting records. This all functionality is presented to the user with a simple API around standard Scala's case classes. 

##Supported databases
Currently SORM supports MySQL and H2 databases. Support for other popular DBs is comming.

##Supported Scala versions
2.10.0-RC1 and later

##Maven support

SORM is distributed in the Maven Central, here's a dependency to the latest release version:

    <dependency>
      <groupId>com.github.nikita-volkov</groupId>
      <artifactId>sorm</artifactId>
      <version>0.2.0</version>
    </dependency>

##A quick glance
Declare a model:

    case class Artist ( name : String, genres : Set[Genre] )
    case class Genre ( names : String ) 

Create a sorm instance, which automatically generates the schema:

    val db 
      = new sorm.Instance(
          entities = Set() + sorm.Entity[Artist]() + sorm.Entity[Style](),
          url = "jdbc:h2:mem:test",
          user = "",
          password = "",
          initMode = sorm.InitMode.Create
        )

Open a single connection:
    
    val cx = db.connection()

Store values in a db:

    val metal = cx.save( Genre("Metal") )
    val rock = cx.save( Genre("Rock") )
    cx.save( Artist("Metallica", Set() + metal + rock) )
    cx.save( Artist("Dire Straits", Set() + rock) )

Retrieve a value from a db:

    val metallica = cx.access[Artist].whereEqual("name", "Metallica").fetchOne()

> You can find a more comprehensive tutorial on the [Wiki](https://github.com/nikita-volkov/sorm/wiki/Tutorial).

##Learn more

For detailed info please visit the [Wiki](https://github.com/nikita-volkov/sorm/wiki) or learn the [API](http://nikita-volkov.github.com/sorm/api/) (you're really interested in the contents of a plain `sorm._` package only).

##Support

Support is provided at [StackOverflow](http://stackoverflow.com/). Go ahead and ask your questions under a tag "sorm".

##Issues

Please post any issues you come across in the [issues](https://github.com/nikita-volkov/sorm/issues) section.

##Contribution
    
It is a very large project, and any kind of contribution is much appreciated. So if you find anything that you think SORM could evolve on, go ahead and fork and leave your pull requests. Currently, the most wanted contributions are drivers for other DBRMs.
