package sorm.query

import sext.Sext._

import sorm.mappings._
import sorm.persisted._

import sorm.abstractSql.{AbstractSql => AS}
import sorm.abstractSql.Combinators._
import Query._
import Operator._
import com.weiglewilczek.slf4s.Logging

object AbstractSqlComposition extends Logging {

//  def resultSetSelect
//    ( query : Query )
//    : AS.Statement
//    = selectWithOrder(query) &&!
//      ( limitSelect(query) ++
//        query.where.map{filtersStatement} reduceOption ( _ & _ ) )
//
//  def selectWithOrder
//    ( query : Query )
//    : AS.Select
//    = (query.mapping.abstractSqlResultSetSelect /: query.order.notEmpty){ case (s, o) =>
//        s.copy(
//          order
//            = o.map{ case Order(m, r) =>
//                AS.Order(
//                  m.containerTableMapping.get.abstractSqlTable,
//                  m.memberName,
//                  r
//                )
//              }
//        )
//      }

  def limitSelect
    ( query : Query )
    : Option[AS.Statement]
    = query
        .satisfying{q => q.limit.nonEmpty || q.offset != 0}
        .map{ q =>
          q.mapping.root.primaryKeySelect
            .copy(
              limit
                = q.limit,
              offset
                = q.offset
            )
        }

  def filtersStatement
    ( where : Where )
    : AS.Statement
    = where match {

        case And(l, r) =>
          filtersStatement(l) & filtersStatement(r)

        case Or(l, r) =>
          filtersStatement(l) | filtersStatement(r)

        case Filter(Equal, m, v) =>
          equaling(m, v)

        case Filter(NotEqual, m, v) =>
          notEqualing(m, v)

        case Filter(Larger, m: ValueMapping, v) =>
          logger.warn("`Larger` filter is not tested")
          comparing( m, AS.Larger, v )

        case Filter(LargerOrEqual, m: ValueMapping, v) =>
          logger.warn("`LargerOrEqual` filter is not tested")
          comparing( m, AS.LargerOrEqual, v )

        case Filter(Smaller, m: ValueMapping, v) => 
          logger.warn("`Smaller` filter is not tested")
          comparing( m, AS.Smaller, v )

        case Filter(SmallerOrEqual, m: ValueMapping, v) =>
          logger.warn("`SmallerOrEqual` filter is not tested")
          comparing( m, AS.SmallerOrEqual, v )

        case Filter(Like, m: ValueMapping, v) => 
          logger.warn("`Like` filter is not tested")
          comparing( m, AS.Like, v )

        case Filter(NotLike, m: ValueMapping, v) => 
          logger.warn("`NotLike` filter is not tested")
          comparing( m, AS.NotLike, v )

        case Filter(Regex, m: ValueMapping, v) => 
          logger.warn("`Regex` filter is not tested")
          comparing( m, AS.Regexp, v )

        case Filter(NotRegex, m: ValueMapping, v) =>
          logger.warn("`NotRegexp` filter is not tested")
          comparing( m, AS.NotRegexp, v )

        case Filter(In, m, v: Iterable[_]) => 
          if( v.nonEmpty ) v.map(equaling(m, _)).reduce(_ | _)
          else everFalse(m)

        case Filter(NotIn, m, v: Iterable[_]) => 
          logger.warn("`NotIn` filter is not tested")
          if( v.nonEmpty ) v.map(notEqualing(m, _)).reduce(_ & _)
          else everTrue(m)

        case Filter(Contains, m: SeqMapping, v) => 
          logger.warn("`Contains` filter is not tested")
          empty(m) && including(m, Iterable(v)) 

        case Filter(Contains, m: SetMapping, v) => 
          logger.warn("`Contains` filter is not tested")
          empty(m) && including(m, Iterable(v)) 

        case Filter(Contains, m: MapMapping, v) => 
          logger.warn("`Contains` filter is not tested")
          empty(m) && including(m, Iterable(v)) 

        case Filter(Constitutes, m: SeqMapping, v) => ???

        case Filter(Constitutes, m: SetMapping, v) => ???

        case Filter(Constitutes, m: MapMapping, v) => ???

        case Filter(Includes, m: SeqMapping, v: Iterable[_]) => 
          logger.warn("`Includes` filter is not tested")
          empty(m) && including(m, v)

        case Filter(Includes, m: SetMapping, v: Iterable[_]) => 
          logger.warn("`Includes` filter is not tested")
          empty(m) && including(m, v)

        case Filter(Includes, m: MapMapping, v: Iterable[_]) => 
          logger.warn("`Includes` filter is not tested")
          empty(m) && including(m, v)

        case Filter(f, m, v) =>
          throw new Exception(
            s"Filter `$f` is not supported for mapping `$m`"
          )
      }


}