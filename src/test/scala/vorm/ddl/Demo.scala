package vorm.ddl

import vorm._
import reflection._
import structure._
import mapping._
import extensions._
import query._

object Demo extends App {

  import samples.ArtistModelWithIds._
  import structure.Sample._


  val mapping
    = mappings(Reflection[Artist])

  mapping.properties("names").asInstanceOf[TableMapping].table.ddl.trace()

}
