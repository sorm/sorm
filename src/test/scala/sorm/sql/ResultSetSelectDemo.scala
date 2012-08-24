package sorm.sql

import sorm._
import extensions._
import Sql._

object ResultSetSelectDemo extends App {
  
  implicit def some ( x : String ) = Some(x)
  implicit def table ( x : String ) = Table(x)
  implicit def value ( x : Any ) = Value(x)

  val primaryKeySelect
    = Select(
          what
            = Column("id", "t0") ::
              Nil,
          from
            = From("artist", "t0"),
          join
            = Join( "artist$names", "t1", Seq( Column("p_id", "t1") → Column("id", "t0") ) ) ::
              Join( "artist$names", "t2", Seq( Column("p_id", "t2") → Column("id", "t0") ) ) ::
              Join( "style", "t3", Seq( Column("id", "t3") → Column("v_id", "t2") ) ) ::
              Join( "style$names", "t4", Seq( Column("p_id", "t4") → Column("id", "t3") ) ) ::
              Join( "name", "t5", Seq( Column("id", "t5") → Column("v_id", "t4") ) ) ::
              Join( "name", "t6", Seq( Column("id", "t6") → Column("v_id", "t2") ) ) ::
              Nil,
          where
            = Some(
                CompositeCondition(
                  CompositeCondition(
                      Comparison( Column("value", "t5"), "hard rock", Equal ),
                      Comparison( Column("value", "t6"), Seq("Nirvana", "Metallica", "Kino"), In ),
                      And
                    ),
                  Comparison( Column("value", "t5"), "pop", Equal ),
                  Or
                )
              ),
          having
            = Some(
                Comparison(
                  Count( Column("id", "t3") :: Nil, true ),
                  1,
                  Larger
                )
              ),
          groupBy
            = Column("id", "t0") :: Nil,
          limit
            = Some(4),
          orderBy
            = OrderBy( Column("value", "t6") ) ::
              Nil

        )

  val resultSetSelect
    = Select(
          what
            = Column("id", "t0") ::
              Column("id", "t3") ::
              Column("id", "t5") ::
              Column("locale", "t5") ::
              Column("value", "t5") ::
              Column("id", "t6") ::
              Column("locale", "t6") ::
              Column("value", "t6") ::
              Nil,
          from
            = From("artist", "t0"),
          join
            = Join( "artist$names", "t1", Seq( Column("p_id", "t1") → Column("id", "t0") ) ) ::
              Join( "artist$names", "t2", Seq( Column("p_id", "t2") → Column("id", "t0") ) ) ::
              Join( "style", "t3", Seq( Column("id", "t3") → Column("v_id", "t2") ) ) ::
              Join( "style$names", "t4", Seq( Column("p_id", "t4") → Column("id", "t3") ) ) ::
              Join( "name", "t5", Seq( Column("id", "t5") → Column("v_id", "t4") ) ) ::
              Join( "name", "t6", Seq( Column("id", "t6") → Column("v_id", "t2") ) ) ::
              Join( primaryKeySelect, "t7", Seq( Column("id", "t7") → Column("id", "t0") ), JoinKind.Right ) ::
              Nil
        )

  import StandardRendering._

  resultSetSelect.template.trace()
  resultSetSelect.data.trace()
}
