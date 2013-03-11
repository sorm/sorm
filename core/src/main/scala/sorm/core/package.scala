package sorm.core
object `package` {

  import language.experimental.macros
  import reflect.runtime.universe._
  import reflect.macros.Context


  sealed trait Persisted {
    val id : Long
  }

  sealed trait Index[ SiteT, FieldsT ] {
    /**
     * 
     */
    val symbols : Seq[Symbol]
  }

  sealed trait Entity[ A ] {
    def toPersisted( value : A, id : Long ) : A with Persisted
  }

  def entityMacro
    [ A : c.WeakTypeTag ]
    ( c : Context )
    ()
    : c.Expr[Entity[A]]
    = {
      import c.universe._
      ???
    }

  def indexMacro
    [ SiteT : c.WeakTypeTag, FieldsT : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ SiteT => FieldsT ] )
    : c.Expr[ Index[ SiteT, FieldsT ] ]
    = ??? 

  /**
   * Exports of this module into the public API (`sorm._`)
   */
  trait Exports {

    def entity[ A ]() : Entity[ A ] = macro entityMacro[ A ]

    def index[ SiteT, FieldsT ]( f : SiteT => FieldsT ) : Index[ SiteT, FieldsT ]
      = macro indexMacro[ SiteT, FieldsT ]

  }

}
