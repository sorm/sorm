package sorm.mappings

import sorm._

// or ContainerRowParsing
trait Parsing {

  //  doesn't pass the pks downstream
  // def value ( data : Stream[_] ) : _  
  //  so we need to pass the whole row during parsing downstream
//  def valueFromContainerRow ( data : Map[String, Any], pk : Map[String, Any] ) : Any
  def valueFromContainerRow ( data : String => Any ) : Any

}
