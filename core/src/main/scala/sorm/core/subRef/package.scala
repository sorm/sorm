package sorm.core.subRef

import reflect.runtime.universe._

/**
 * A reference from a context type to its subfield. E.g., `context.field` or 
 * `context.field.field` and so on.
 *
 * @tparam Context A type of the context object.
 * @tparam Value A type of the referred value.
 * @param contextSymbol A symbol of a context object.
 * @param valueSymbol A symbol of the referred object. Must have the 
 * `contextSymbol` in its `owner`s chain.
 */
case class SubRef
  [ Context, Value ]
  ( contextSymbol : Symbol,
    valueSymbol : Symbol )


sealed trait SubRefs
  [ Context, Value ]

case class SubRefsValue
  [ A, B, C ]
  ( subRef : SubRef[ A, B ], 
    tail : SubRefs[ A, C ] )
  extends SubRefs[ A, (B, C) ]

case class SubRefsNil[ A ] extends SubRefs[ A, Unit ]
