package sorm.core.api.mapping

// trait DefinesMappings [driver] {
//   implicit def mapping[a]()
// }

// /**
//  * A set of rules of how a certain type in a certain tree position relative to
//  * the root queryable entity gets treaten.
//  */
// trait Mapping [driver, operation, position, value] {
  
// }

/**
 * A position of a type relative to a root entity.
 * Encodable on a type-level, so that it could be used to determine type-class instances,
 * which in turn should provide compile-time protection for support of certain operations
 * on types at certain position.
 */
sealed trait Position
object Position {
  class Member[ parent <: Position, index <: shapeless.Nat ] extends Position
  class Root extends Position
}

// NOTE: No need for "driver", since it is already determined by the place of implementation.
sealed trait Operation[ value, position <: Position ]
object Operation {
  class Create
//  class Order extends Operation
}

