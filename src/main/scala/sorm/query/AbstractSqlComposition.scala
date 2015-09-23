package sorm.query

import sext._, embrace._

import sorm.mappings._
import sorm.persisted._

import sorm.abstractSql.{AbstractSql => AS}
import sorm.abstractSql.Combinators._
import Query._
import com.typesafe.scalalogging.{StrictLogging => Logging}

object AbstractSqlComposition extends Logging {

  def primaryKeySelect
    ( query : Query )
    : AS.Statement
    = (query.mapping.primaryKeySelect /: query.order.notEmpty){ case (s, o) =>
        s.copy(
          order
            = o.map{ case Order(m, r) =>
                AS.Order(
                  m.containerTableMapping.get.abstractSqlTable,
                  m.memberName,
                  r
                )
              }
        )
      } &&
      ( limitSelect(query) ++
        query.where.map{filtersStatement} reduceOption ( _ & _ ) )

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
          comparing( m, AS.Larger, v )

        case Filter(LargerOrEqual, m: ValueMapping, v) =>
          comparing( m, AS.LargerOrEqual, v )

        case Filter(Smaller, m: ValueMapping, v) => 
          comparing( m, AS.Smaller, v )

        case Filter(SmallerOrEqual, m: ValueMapping, v) =>
          comparing( m, AS.SmallerOrEqual, v )

        case Filter(Like, m: ValueMapping, v) => 
          comparing( m, AS.Like, v )

        case Filter(NotLike, m: ValueMapping, v) => 
          comparing( m, AS.NotLike, v )

        case Filter(Regex, m: ValueMapping, v) => 
          comparing( m, AS.Regexp, v )

        case Filter(NotRegex, m: ValueMapping, v) =>
          comparing( m, AS.NotRegexp, v )

        case Filter(In, m, v: Iterable[_]) => 
          if( v.nonEmpty ) v.map(equaling(m, _)).reduce(_ | _)
          else everFalse(m)

        case Filter(NotIn, m, v: Iterable[_]) => 
          if( v.nonEmpty ) v.map(notEqualing(m, _)).reduce(_ & _)
          else everTrue(m)

        case Filter(Contains, m: SeqMapping, v) => 
          empty(m) && including(m, Iterable(v)) 

        case Filter(Contains, m: SetMapping, v) => 
          empty(m) && including(m, Iterable(v)) 

        case Filter(Contains, m: MapMapping, v) => 
          empty(m) && including(m, Iterable(v)) 

        case Filter(Constitutes, m: SeqMapping, v) => ???

        case Filter(Constitutes, m: SetMapping, v) => ???

        case Filter(Constitutes, m: MapMapping, v) => ???

        case Filter(Includes, m: SeqMapping, v: Iterable[_]) => 
          empty(m) && including(m, v)

        case Filter(Includes, m: SetMapping, v: Iterable[_]) => 
          empty(m) && including(m, v)

        case Filter(Includes, m: MapMapping, v: Iterable[_]) => 
          empty(m) && including(m, v)

        case Filter(f, m, v) =>
          throw new Exception(
            s"Filter `$f` is not supported for mapping `$m`"
          )
      }


}
