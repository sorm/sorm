/**
 * A Select-query template AST, which abstracts away from table aliases and 
 * from-join management, 
 * using table references which represent specific tables in a query and 
 * information on how they should be joined.
 */
package sorm.relational.joinExpressions

object templates {

  case class Column ( name: String, from: From )

  sealed trait From
  object From {
    case class Root
      ( name: String )
      extends From
    case class Join
      ( name: String, parent: From, bindings: Seq[(String, String)] )
      extends From
  }

  sealed trait Where
  object Where {
    case class Fork ( left: Where, right: Where, or: Boolean ) extends Where
    case class Comparison
      ( column: Column, operator: Operator, value: Expression, negative: Boolean )
      extends Where
  }

  sealed trait Operator
  object Operator {
    case object Equal extends Operator
    case object Larger extends Operator
    case object Smaller extends Operator
    case object Like extends Operator
    case object Regexp extends Operator
    case object In extends Operator
  }

  sealed trait Expression
  object Expression {
    case object Placeholder extends Expression
    // case class Select( select: sorm.relational.queryJoinTemplates.Select ) extends Expression
  }

  // case class Select
  //   ( what: What,
  //     where: Where,
  //     having: Having,
  //     groupBy: GroupBy,
  //     limit: Option[Int] = Nothing,
  //     offset: Int = 0 )

  // type GroupBy =

}

object typeClasses {

  import templates._
  import reflect.runtime.{universe => ru}
  import sorm.core._

  trait ToFrom[ a ] {
    def toFrom: From
  }
  object ToFrom {

//    implicit def i1[ path ]( implicit pathToType: ToType[path] ) =
//      new ToFrom[path] {
//        val toFrom = {
//          val name = {
//            val typeName = rootTT.tpe.typeSymbol.name.decoded
//            ddlEncode(typeName)
//          }
//          From.Root(name)
//        }
//      }

    implicit def i1
      [ root ]
      ( implicit rootTT: ru.TypeTag[root] )
      =
      new ToFrom[ TypePath.Root[ root ] ] {
        val toFrom = {
          val name = {
            val typeName = rootTT.tpe.typeSymbol.name.decoded
            ddlEncode(typeName)
          }
          From.Root(name)
        }
      }
    // implicit def i2
    //   [ path <: TypePath.Member[root, parent, index],
    //     root, 
    //     parent <: TypePath[root], 
    //     index <: shapeless.Nat ]
    //   ( implicit 
    //       pathToType: ToType[path],
    //       parentToFrom: ToFrom[parent],
    //        )
    //   =
    //   new ToFrom[ path ] {
    //     val toFrom = {
    //       val name = {
    //         val typeName = {
    //           rootTT.tpe.members.withFilter(_.isTerm).withFilter(!_.isMethod)
    //         }
    //         ???
    //       }
    //       val parent = parentToFrom.toFrom
    //       val bindings = ???
    //       From.Join(name, parent, bindings)
    //     }
    //   }

  }

  trait ToColumn[ a ] {
    def toColumn: Column
  }
  object ToColumn {

    implicit def i1
      [ root, parent <: TypePath[ root ], index <: shapeless.Nat ]
      ( implicit rootTT: ru.TypeTag[ root ] )
      =
      new ToColumn[ TypePath.Property[ root, parent, index ] ] {
        val toColumn = ???
      }

  }

  private def ddlEncode ( string : String ) : String = {
    import com.google.common.base.CaseFormat._
    UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
  }

}
