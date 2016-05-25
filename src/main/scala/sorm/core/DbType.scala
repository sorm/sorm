package sorm.core

sealed trait DbType
object DbType {

  case object Mysql extends DbType
  case object H2 extends DbType
  case object Hsqldb extends DbType
  case object Sqlite extends DbType
  case object Postgres extends DbType
  case object Oracle extends DbType
  case object Sqlserver extends DbType
  case object Derby extends DbType
  case object Sybase extends DbType
  case object Db2 extends DbType

  def byUrl
    ( u : String )
    : DbType
    = u match {
        case u if u.startsWith("jdbc:mysql:") => Mysql
        case u if u.startsWith("jdbc:h2:") => H2
        case u if u.startsWith("jdbc:hsqldb:") => Hsqldb
        case u if u.startsWith("jdbc:sqlite:") => Sqlite
        case u if u.startsWith("jdbc:postgresql:") => Postgres
        case u if u.startsWith("jdbc:oracle:") => Oracle
        case u if u.startsWith("jdbc:sqlserver:") => Sqlserver
        case u if u.startsWith("jdbc:derby:") => Derby
        case u if u.startsWith("jdbc:sybase:") => Sybase
        case u if u.startsWith("jdbc:db2:") => Db2
      }

  def driverClass
    ( t : DbType )
    = t match {
        case DbType.Mysql    => "com.mysql.jdbc.Driver"
        case DbType.Postgres => "org.postgresql.Driver"
        case DbType.H2       => "org.h2.Driver"
        case DbType.Sqlite   => "org.sqlite.JDBC"
        case DbType.Hsqldb   => "org.hsqldb.jdbcDriver"
        case DbType.Derby    => "org.apache.derby.jdbc.EmbeddedDriver"
        case DbType.Oracle   => "oracle.jdbc.driver.OracleDriver"
        case _               => ???
      }
}
