package vorm

package sql {

  case class Select
    ( what : Seq[SelectObject],
      from : From,
      join : Seq[Join] = Nil,
      where : Option[Clause] = None,
      orderBy : Seq[OrderByClause] = Nil,
      limit : Option[Int] = None,
      offset : Option[Int] = None,
      groupBy : Seq[GroupByObject] = Nil,
      having : Option[Clause] = None )
    extends FromObject with JoinObject
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
    }


  case class OrderByClause
    ( what : Column,
      desc : Boolean = false )

  trait SelectObject
  trait GroupByObject

  case class Table
    ( name : String )
    extends FromObject with JoinObject

  case class From
    ( what : FromObject,
      as : Option[String] = None )

  trait FromObject

  case class Join
    ( what : JoinObject,
      as : Option[String],
      on : Seq[(Column, Column)] = Nil,
      kind : JoinKind = JoinKind.Left )

  trait JoinObject

  trait JoinKind
  object JoinKind {
    object Left  extends JoinKind
    object Right extends JoinKind
    object Inner extends JoinKind
    object Outer extends JoinKind
  }


  case class Column
    ( name : String,
      table : Option[String] )
    extends SelectObject 
    with ConditionObject
    with GroupByObject

  case class Count
    ( what : Seq[Column],
      distinct : Boolean = false )
    extends SelectObject 
    with ConditionObject
    with GroupByObject



  trait Clause
  trait ConditionObject

  object Clause {
    
    trait Composite extends Clause {
      def left : Clause
      def right : Clause
    }

    case class And
      ( left : Clause,
        right : Clause )
      extends Composite

    case class Or
      ( left : Clause,
        right : Clause )
      extends Composite

    trait Condition extends Clause
    
    case class Equals
      ( left : ConditionObject,
        right : ConditionObject )
      extends Condition

  }

  case class Value
    ( value : Any )
    extends ConditionObject



}