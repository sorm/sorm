package sorm

import sorm._
import reflection._
import structure._
import mapping._
import jdbc._
import sext._
import ddl._
import java.sql.ResultSet

package object resultSet {

  implicit def resultSetParsingAdapter ( rs : ResultSet )
    = new ResultSetParsingAdapter(rs)


}


