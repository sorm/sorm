#SORM. An elegant and scalable way to do persistence in Scala

SORM is a purely Scala-oriented object-relational mapping framework designed to eliminate boilerplate and maximize productivity. It is absolutely abstracted from relational side automatically creating database tables, emitting queries, inserting, updating and deleting records. This all functionality is presented to the user with a simple API around standard Scala's case classes. 

For more info, tutorials and documentation please visit the [official site](http://sorm-framework.org).

##Supported databases

* MySQL
* PostgreSQL
* H2
* HSQLDB

##Supported Scala versions

2.10.0-RC1, 2.10.0-RC2

##Maven

SORM is distributed in the Maven Central, here's a dependency to the latest release version:

    <dependency>
      <groupId>com.github.nikita-volkov</groupId>
      <artifactId>sorm</artifactId>
      <version>0.3.1</version>
    </dependency>

##Contribution

Any kind of contribution is much appreciated. If you find anything that you think SORM could evolve on, go ahead and fork it! 
