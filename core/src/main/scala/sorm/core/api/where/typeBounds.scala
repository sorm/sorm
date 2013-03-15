/**
 * Driver-specific operations support type-bounds. 
 *
 * E.g., there's no way to implement a `regex` operator in CouchDB, so with 
 * help of driver-specific type support the `regex` operator will get statically
 * protected from being used.
 */
package sorm.core.api.where

trait EqualsSupport[ Driver, T ]
trait NotLargerSupport[ Driver, T ]
trait RegexSupport[ Driver, T ]
trait ExistsSupport[ Driver, T ]

trait DriverSpecificOperationsSupport[ Driver ] {
  type DriverEqualsSupport[ T ] = EqualsSupport[ Driver, T ]
  type DriverNotLargerSupport[ T ] = NotLargerSupport[ Driver, T ]
  type DriverRegexSupport[ T ] = RegexSupport[ Driver, T ]
  type DriverExistsSupport[ T ] = ExistsSupport[ Driver, T ]
}
