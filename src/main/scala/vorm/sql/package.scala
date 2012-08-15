package vorm

import extensions._

package object sql {

  def alias ( x : Int ) = ( 97 + x ).toChar.toString

  trait Renderable {
    def rendering : String
    def data : Seq[Any]
    protected def quote
      ( s : String )
      = s
  }

  case class Select
    ( what : Seq[SelectObject],
      from : From,
      join : Seq[Join] = Nil,
      where : Option[Clause] = None,
      groupBy : Seq[GroupByObject] = Nil,
      having : Option[Clause] = None,
      orderBy : Seq[OrderByClause] = Nil,
      limit : Option[Int] = None,
      offset : Option[Int] = None )
    extends FromObject 
    with JoinObject
    with Renderable
    {
      /**
       * Drops orphan joins
       */
      def optimized
        : Select
        = {
          val refs
            : Set[String]
            = {
              val whatRefs
                = what.view collect { case Column(_, Some(r)) ⇒ r }
              val fromRef
                = from.as
              val whereRefs
                = ???
              val groupByRefs
                = groupBy collect { case Column(_, Some(r)) ⇒ r }
              val havingRefs
                = ???

              Set() ++ whatRefs ++ fromRef ++ whereRefs ++ groupByRefs ++ havingRefs
            }
          
          def f
            ( s : Select )
            : Select
            = {
              val joinRefs
                = s.join.view flatMap {
                    _.on collect { case (_, Column(_, Some(r))) ⇒ r }
                  }

              val allRefs
                = refs ++ joinRefs

              val filtered 
                = s.join filter { 
                    _.as map { allRefs contains _ } getOrElse false 
                  }

              if( filtered == s.join )
                s
              else
                f( s copy ( join = filtered ) )
            }

          def withSubSelectsOptimized
            ( s : Select )
            = s.copy(
                  join 
                    = s.join map { j ⇒ 
                        j.what match {
                          case s : Select ⇒ j.copy(s.optimized)
                          case _ ⇒ j 
                        }
                      }
                )

          withSubSelectsOptimized( f(this) ) 
         
        }

      /**
       * Filter results of current select ...
       */
      def narrow
        ( s : Select )
        : Select
        = s match { 
            case s 
              if s.from == from
              ⇒ s.what match { 
                  case w : Seq[Column] 
                    if w.groupBy{_.table}.size == 1
                    ⇒ copy(
                        join
                          = join :+
                            Join(
                              what = s,
                              as = Some( alias(join.length + 1) ),
                              on = w.view.map{_.name}
                                    .map{ n ⇒ Column(n, alias(join.length + 1).some) → 
                                              Column(n, alias(0).some) }
                                    .toList,
                              kind = JoinKind.Right
                            )
                      )

                }
          }


      /**
       * !! Not tested at all
       */
      def intersectionWith
        ( s : Select )
        : Select
        = if( s.what == what )  
            if( s == this )
              this
            else if( s.from == from &&
                     s.join == join &&
                     s.orderBy == orderBy &&
                     s.limit == limit &&
                     s.offset == offset &&
                     s.groupBy == groupBy &&
                     ( having == None || s.having == None ) )
              copy(
                  where
                    = (where ++ s.where) reduceOption Clause.And,
                  having
                    = (having ++ s.having) reduceOption Clause.And
                )
            else
              copy(
                  join
                    = join :+
                      Join(
                          what = s,
                          as = Some("t" + (join.length + 1)),
                          on = (what zip s.what)
                                .asInstanceOf[Seq[(Column, Column)]],
                          kind = JoinKind.Inner
                        )
                )
          else
            throw new MatchError("Unmergeable selects")

      /**
       * !! Not tested at all
       */
      def unionWith
        ( s : Select )
        : Select
        = if( s.what == what )  
            if( s == this )
              this
            else if( s.from == from &&
                     s.join == join &&
                     s.orderBy == orderBy &&
                     s.limit == limit &&
                     s.offset == offset &&
                     s.groupBy == groupBy &&
                     ( having == None || s.having == None ) )
              copy(
                  where
                    = (where ++ s.where) reduceOption Clause.Or,
                  having
                    = (having ++ s.having) reduceOption Clause.Or
                )
            else
              copy(
                  join
                    = join :+
                      Join(
                          what = s,
                          as = Some("t" + (join.length + 1))
                        )
                )
          else
            throw new MatchError("Unmergeable selects")
      
      def rendering
        = "SELECT\n" +
          ( what.view.map{_.rendering}.mkString(", ") +
            "\n" + from.rendering +
            join
              .view
              .map{ _.rendering }
              .mkString("\n")
              .satisfying{ ! _.isEmpty }
              .map{"\n" + _}
              .getOrElse("") +
            where
              .map{ _.rendering }
              .map{ "\nWHERE " + _.indent("WHERE ".length).trim }
              .getOrElse("") +
            groupBy
              .view
              .map{ _.rendering }
              .mkString(", ")
              .satisfying{ ! _.isEmpty }
              .map{ "\nGROUP BY " + _ }
              .getOrElse("") +
            having
              .map{ _.rendering }
              .map{ "\nHAVING " + _.indent("HAVING ".length).trim }
              .getOrElse("") +
            orderBy
              .map{ _.rendering }
              .mkString(", ")
              .satisfying{ ! _.isEmpty }
              .map{ "\nORDER BY " + _ }
              .getOrElse("") +
            limit
              .map{ "\nLIMIT " + _ }
              .getOrElse("") +
            offset
              .map{ "\nOFFSET " + _ }
              .getOrElse("") )
            .indent(2)

      def data
        = ( what ++: from +: join ++: where ++: groupBy ++: having ++:
            orderBy ++: Nil )
            .flatMap{_.data}
    }


  case class OrderByClause
    ( what : Column,
      desc : Boolean = false )
    extends Renderable
    {
      def rendering
        = if( desc ) what.rendering + " DESC"
          else what.rendering
      def data 
        = what.data
    }

  trait SelectObject extends Renderable
  trait GroupByObject extends Renderable

  case class Table
    ( name : String )
    extends FromObject 
    with JoinObject
    {
      def rendering = quote(name)
      def data = Nil
    }

  case class From
    ( what : FromObject,
      as : Option[String] = None )
    extends Renderable
    {
      def rendering
        = "FROM\n" +
          (
            ( what match {
                case Table(name) ⇒ quote(name)
                case r : Renderable ⇒ "(\n" + r.rendering.indent(2) + "\n)"
              } ) +
            as.map{ "\nAS " + quote(_) }
              .getOrElse("")
            ) 
            .indent(2)
      def data 
        = what.data
    }
  object From {
    def apply ( what : FromObject, as : String ) : From
      = apply( what, Some(as) )
  }

  trait FromObject extends Renderable

  case class Join
    ( what : JoinObject,
      as : Option[String] = None,
      on : Seq[(Column, Column)] = Nil,
      kind : JoinKind = JoinKind.Left )
    extends Renderable
    {
      def rendering
        = ( kind match {
              case JoinKind.Left ⇒ "LEFT JOIN "
              case JoinKind.Right ⇒ "RIGHT JOIN "
              case JoinKind.Inner ⇒ "INNER JOIN "
            } ) + "\n" +
          (
              ( what match {
                  case Table(name) ⇒ quote(name)
                  case r : Renderable ⇒ "( " + r.rendering.indent(2).trim + " )"
                } ) +
              as.map{ "\nAS " + quote(_) }
                .getOrElse("") +
              on.map{ case (l, r) ⇒ l.rendering + " = " + r.rendering }
                .mkString(" AND\n")
                .satisfying{ ! _.isEmpty }
                .map{ "\nON " + _.indent("ON ".length).trim }
                .getOrElse("")
            )
            .indent(2)

      def data 
        = what.data
    }

  trait JoinObject extends Renderable


  trait JoinKind
  object JoinKind {
    object Left  extends JoinKind
    object Right extends JoinKind
    object Inner extends JoinKind
  }


  case class Column
    ( name : String,
      table : Option[String] = None )
    extends SelectObject 
    with ConditionObject
    with GroupByObject
    {
      def rendering
        = table
            .map{ quote(_) + "." }
            .getOrElse("") + 
          quote(name)
      def data
        = Nil
    }
  object Column {
    def apply ( name : String, table : String ) : Column
      = apply( name, Some(table) )
  }

  case class Count
    ( what : Seq[Column],
      distinct : Boolean = false )
    extends SelectObject 
    with ConditionObject
    with GroupByObject
    {
      def rendering
        = "COUNT(" +
          ( if( distinct ) "DISTINCT " 
            else "" ) +
          what.view.map{ _.rendering }
            .satisfying1{_.size > 1}
            //  a trick from here: http://h2-database.66688.n3.nabble.com/COUNT-DISTINCT-several-columns-doesn-t-work-td3721939.html
            .left.map{"(" + _.mkString(", ") + ")"}
            .right.map{_.mkString(", ")}
            .merge
          ")"
      def data
        = Nil
    }

  trait Clause extends Renderable
  trait ConditionObject extends Renderable

  object Clause {
    
    abstract class Composite 
      ( operator : String )
      extends Clause 
      {
        def left : Clause
        def right : Clause
        private def subClauseSql
          ( c : Clause )
          : String
          = c match {
              case c : Composite ⇒ "( " + c.rendering.indent("( ".length).trim + " )"
              case c ⇒ c.rendering
            }
        def rendering
          = subClauseSql(left) + " " + operator + "\n" + subClauseSql(right)
        def data
          = left.data ++ right.data
      }

    case class And
      ( left : Clause,
        right : Clause )
      extends Composite("AND")

    case class Or
      ( left : Clause,
        right : Clause )
      extends Composite("OR")

    abstract class Condition 
      ( operator : String )
      extends Clause 
      {
        def left : ConditionObject
        def right : ConditionObject
        def rendering
          = left.rendering + " " + operator + " " + right.rendering
        def data
          = left.data ++ right.data
      }
    
    case class Equals 
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition(
        right match {
          case Value(null) => "IS"
          case _ => "="
        }
      )
    
    case class NotEquals
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition(
        right match {
          case Value(null) => "IS NOT"
          case _ => "!="
        }
      )

    case class Larger
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition(">")

    case class LargerIncluding
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition(">=")

    case class Smaller
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition("<")

    case class SmallerIncluding
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition("<=")

    case class Like
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition("LIKE")

    case class Regex
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition("REGEXP")
    
    case class In
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition("IN")
  }

  case class Value
    ( value : Any )
    extends ConditionObject
    {
      def rendering
        = "?"
      def data
        = Vector(value)
    }



}