package vorm.api

import org.joda.time.DateTime
import java.sql.DriverManager

import vorm._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import extensions._

class Instance
  ( url : String,
    user : String,
    password : String,
    entities : Traversable[Entity[_]],
    mode : Mode = Mode.Create )
  {
    protected val api
      = new ConnectionAdapter(DriverManager.getConnection(url, user, password))
          with SaveAdapter

    protected val mappings
      = {
        val settings
          = entities.view.map{ e => e.reflection -> e.settings }.toMap

        settings.keys
          .zipBy{ new EntityMapping(None, _, settings) }
          .toMap
      }

    mode match {
      case Mode.DropCreate =>
        ???
//        doesn't work because dropping requires dropping slave tables first
//        for( m <- mappings.values ){
//          try {
//            api.executeUpdate(
//              Statement("DROP TABLE `" + m.tableName + "`")
//            )
//          } catch {
//            case _ : Throwable =>
//          }
//        }
//        for( s <- statements(mappings.values) ){
//          api.executeUpdate(s)
//        }
      case Mode.Create =>
        for( s <- Create.statements(mappings.values) ){
          api.executeUpdate(s)
        }
    }

    private def mapping
      [ T : TypeTag ]
      = mappings.get(Reflection[T]) match {
          case Some(m) => m
          case None => 
            throw new RuntimeException( "Entity `" + Reflection[T].name + "` is not registered" )
        }

    /**
     * Current time at DB server
     */
    def date : DateTime
      = ???

    def save
      [ T <: AnyRef : TypeTag ]
      ( value : T )
      : T with Persisted
      = api.saveEntityAndGetIt( value, mapping[T] )
          .asInstanceOf[T with Persisted]

  }