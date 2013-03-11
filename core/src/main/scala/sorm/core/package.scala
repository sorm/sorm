package sorm.core

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context

object `package` {

  sealed trait Persisted {
    val id : Long
  }

  sealed trait Key[ SiteT, FieldsT ] {
    /**
     * 
     */
    val symbols : Seq[ Symbol ]
    // TODO: implement hashCode and eq
  }

  sealed trait Entity[ A ] {
    def toPersisted( value : A, id : Long ) : A with Persisted
    // TODO: implement hashCode and eq
  }

  /**
   * Exports of this module into the public API, e.g. `sorm._`.
   */
  trait Exports {

    type Persisted = sorm.core.Persisted

    def entity[ A ]
      ( indexed : Set[ Key[ A, _ ] ], unique : Set[ Key[ A, _ ] ] ) 
      : Entity[ A ] 
      = macro Macros.entity[ A ]

    def key[ SiteT, FieldsT ]( f : SiteT => FieldsT ) 
      : Key[ SiteT, FieldsT ]
      = macro Macros.key[ SiteT, FieldsT ]

  }

}

