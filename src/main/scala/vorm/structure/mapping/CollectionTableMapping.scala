package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

abstract class CollectionTableMapping
  extends TableMapping
  {
    def uniqueKeys: Set[Seq[String]] = Set.empty
    def indexes: Set[Seq[String]] = Set.empty
    def autoIncrement: Set[String] = Set.empty
  }
