package vorm.api

object Sandbox extends App {

  val api: API

  case class Genre(
    name: String
  )

  case class Artist(
    name: String,
    genres: Set[Genre]
  )

  val artist = api.all[Artist].filterEquals("name", "Metallica").offset(0).limit(1).head

  val style
    = db.one[Style].where(
        ("names.value" === "pop") and 
        ("names.locale.code" === "en")
      )

  //  ideas:::
  // object DB extends Instance(
  //   Settings("jdbc://")
  //     .withEntity[Artist](primaryKey = Seq("id"), autoIncrement = Set("id"))
  //     .withEntity[Style]
  //     .withEntity[Name]
  //     .withEntity[Locale]
  // )

  object DB 
    extends Instance(
      url 
        = "jdbc://",
      user 
        = "",
      password 
        = "",
      entities 
        = Entity[Artist](primaryKey = Seq("id"), autoIncrement = Set("id")) ::
          Entity[Style]() ::
          Entity[Name]() ::
          Entity[Locale]() ::
          Nil
    )

  DB.save(
    DB.one[Locale].filter("code" equal "ru").get.copy(code = "ru")
  )

  sealed case class Entity
    [ T : TypeTag ]
    ( primaryKey    : Seq[String],
      uniqueKeys    : Set[Seq[String]]  = Set.empty,
      indexes       : Set[Seq[String]]  = Set.empty,
      autoIncrement : Set[String]       = Set.empty )
    {
      // here should be tests on validity of provided data
    }
  object Entity {
    def apply
      [ T : TypeTag ]
      ( uniqueKeys    : Set[Seq[String]]  = Set.empty,
        indexes       : Set[Seq[String]]  = Set.empty,
        autoIncrement : Set[String]       = Set.empty )
      : Entity[T]
      = apply[T](
          primaryKey = Seq("id"),
          autoIncrement = autoIncrement + "id",
          uniqueKeys = uniqueKeys,
          indexes = indexes
        )
  }

}
