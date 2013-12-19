package sorm.core.subRef

import reflect.runtime.universe._



/**
 * A reference from a context type to its subfield. E.g., `context.field` or 
 * `context.field.field` and so on.
 * 
 * @tparam Context A type of the context object.
 * @tparam Value A type of the referred end-value.
 * @param contextType A type object of the context object. 
 * @param subFieldSymbols A list of symbols representing the chain of subfields.
 * 
 * @example
 *   For example, a reference `a.b.c` should be represented as follows:
 *   {{{
 *   SubRef
 *     [ <Type of `a`>, <Type of `c`> ]
 *     ( <rep of type of `a`>, List( <Symbol of field `b` of type of `a`>,
 *                                   <Symbol of field `c` of type of `a.b`> ) )
 *   }}}
 */
// TODO: rename to FieldRef
case class SubRef
  [ Context, Value ]
  ( contextType : Type,
    subFieldSymbols : List[ Symbol ] )



sealed trait SubRefs
  [ Context, Value ]

case class SubRefsValue
  [ A, B, C ]
  ( subRef : SubRef[ A, B ], 
    tail : SubRefs[ A, C ] )
  extends SubRefs[ A, (B, C) ]

case class SubRefsNil[ A ] extends SubRefs[ A, Unit ]
