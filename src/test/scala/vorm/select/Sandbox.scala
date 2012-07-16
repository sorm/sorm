package vorm.select

object Sandbox extends App {
  println(
    sql(
      SelectNode(
        table = "a",
        columns = List("A", "B", "C"),
        mappings = Set(),
        children
          = List(
              SelectNode(
                table = "b",
                columns = List("Z", "X", "a_A", "a_B"),
                mappings = Set(("a_A", "A"), ("a_B", "B")),
                children
                  = List(
                      SelectNode(
                        table = "d",
                        columns = List("Z", "X", "a_A", "a_B"),
                        mappings = Set(("a_A", "A"), ("a_B", "B")),
                        children = Nil
                      ),
                      SelectNode(
                        table = "e",
                        columns = List("Z", "X", "a_A", "a_B"),
                        mappings = Set(("a_A", "A"), ("a_B", "B")),
                        children = Nil
                      )
                    )
              ),
              SelectNode(
                table = "c",
                columns = List("Z", "X", "a_A", "a_B"),
                mappings = Set(("a_A", "A"), ("a_B", "B")),
                children = Nil
              )
          )
      )
    )
  )
}
