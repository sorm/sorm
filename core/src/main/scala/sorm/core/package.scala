package sorm.core

object `package` {
  
  import language.experimental.macros

  sealed trait Persisted {
    val id : Long
  }

  sealed trait Key[ SiteT ] {
    /**
     * 
     */
    val symbols : Seq[ Symbol ]
    // TODO: implement hashCode and eq
  }

  // case class Entity[ A ]( toPersisted : (A, Long) => A with Persisted )
  sealed trait Entity[ A ] {
    val indexed : Set[ Key[ A ] ]
    val unique : Set[ Key[ A ] ]
    def toPersisted( value : A, id : Long ) : A with Persisted
    // TODO: implement hashCode and eq
  }

  /**
   * Exports of this module into the public API, e.g. `sorm._`.
   */
  trait Exports {

    type Persisted = sorm.core.Persisted

    def entity[ A ]
      ( indexed : Set[ Key[ A ] ], unique : Set[ Key[ A ] ] ) 
      : Entity[ A ] 
      = macro Macros.entity[ A ]

    def key[ SiteT, FieldT ]
      ( f : SiteT => FieldT ) 
      : Key[ SiteT ]
      = macro Macros.key1[ SiteT, FieldT ]

    def key[ SiteT, FieldT1, FieldT2 ]
      ( f1 : SiteT => FieldT1,
        f2 : SiteT => FieldT2 ) 
      : Key[ SiteT ]
      = macro Macros.key2[ SiteT, FieldT1, FieldT2 ]

    def key[ SiteT, FieldT1, FieldT2, FieldT3 ]
      ( f1 : SiteT => FieldT1,
        f2 : SiteT => FieldT2,
        f3 : SiteT => FieldT3 ) 
      : Key[ SiteT ]
      = macro Macros.key3[ SiteT, FieldT1, FieldT2, FieldT3 ]

    def key[ SiteT, FieldT1, FieldT2, FieldT3, FieldT4 ]
      ( f1 : SiteT => FieldT1,
        f2 : SiteT => FieldT2,
        f3 : SiteT => FieldT3,
        f4 : SiteT => FieldT4 ) 
      : Key[ SiteT ]
      = macro Macros.key4[ SiteT, FieldT1, FieldT2, FieldT3, FieldT4 ]

  }

}

