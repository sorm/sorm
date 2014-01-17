package sorm.mysql

// import sorm.core.api._

// class MysqlInstance 
//   extends InstanceSelectSupport[ MysqlDriver ]
//   {
//     val driver = new MysqlDriver {}


//   }

// trait MysqlDriver extends DriverSelectSupport {

// }
// object MysqlDriver extends DriverSpecificOperationsSupport[MysqlDriver] {

//   implicit object IntEqualsSupport 
//     extends DriverEqualsSupport[ Int ]

//   implicit object IntNotLargerSupport 
//     extends DriverNotLargerSupport[ Int ]

//   implicit object StringEqualsSupport 
//     extends DriverEqualsSupport[ String ]

//   implicit object StringRegexSupport 
//     extends DriverRegexSupport[ String ]

//   implicit object TraversableExistsSupport 
//     extends DriverExistsSupport[ Traversable[ _ ] ]

// }

// class Mysql {

//   object sql {
//     sealed trait AST
//   }
//   type AST = sql.AST

//   import mapping._
//   // implicit val i0 = 
//   //   new Operation.Order[ Int, Position.Member[_, _] ]{
//   //     // def instruction(value: Int, )
//   //   }
//   implicit val i1 = 
//     new Operation.Equals[ value <: Product <: Persisted, Position.Member[_, _] ]{
//       def updateAST(ast: AST, a: value, b: value): AST
//     }
//   // implicit def i2[a <: Product <: Persisted](implicit t: ru.TypeRep[a], member: Member[a]) =
//   //   new Operation.Create[ value <: Product : Type, ]

// }

// -----------------
// The following approach allows to delegate all the checking to compiler and
// not to bother with it at the time of building.
class Mysql extends QueryJoinTemplatesCompilers {

}


