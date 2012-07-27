package vorm.selectAbstraction

import vorm._
import extensions._
import structure._
import mapping.{Table => TableMapping, Seq => SeqMapping}


/**
 * Should solve the alias management problem
 */
// case class MappingSelect
//   ( mapping : TableMapping,
//     sql : Sql.Select
//     // skeletonJoins : Map[Mapping, Sql.Join]

//     )
//   {

//     // def withClause
//     //   ( c : Clause )
//     //   = 


//     lazy val primaryKey
//       = copy(
//             sql
//               = sql.copy(
//                     what 
//                       = mapping.primaryKeyColumns.view
//                           .map( _.name )
//                           .map( Sql.Column(_, Some(skeletonTablesAliases(this))) )
//                   )
//           )


//     // direct API instead of clauses
//     def rows
//       ( rows : Int )
//       : MappingSelect
//       = ???

//     // def subSelect
//     //   (  )

//     // //  abstraction api over direct api
//     // def equals
//     //   (  )




//     // def equals
//     //   ( v : _ )
//     //   : MappingSelect
//     //   = ( mapping, v ) match {
//     //       case ( m : SeqMapping, v : Seq[_] )
//     //         ⇒ m.select.primaryKey
//     //             .foldFrom(v) { (s, v) ⇒ 
//     //               s.orWhere(
//     //                   Query.Where.Equals(m.child.child, v)
//     //                 )
//     //             }
//     //     }

//     // private def subSelect
//     //   ( w : Query.Where )
//     //   : MappingSelect
//     //   = w match {
//     //       case Query.Where.Equals( m : SeqMapping, v : Seq[_] )
//     //         ⇒ m.select.primaryKey
//     //             .foldFrom(v) { (s, v) ⇒ 
//     //               s.orWhere(
//     //                   Query.Where.Equals(m.child.child, v)
//     //                 )
//     //             }
//     //     }

//     // def andWhere
//     //   ( w : Query.Where )
//     //   : MappingSelect
//     //   = w match {
//     //       case Query.Where.Equals( m : SeqMapping, v : Seq[_] )
//     //         ⇒ ???
//     //     }
//   }
case class MappingSelect
  ( mapping : TableMapping )
  {
    def where
      ( w : Query.Where )
      : MappingSelect
      = w match {
          case Query.Where.Equals( m : SeqMapping, v : Seq[_] )
            ⇒ m.select.primaryKey
                .foldFrom(v) { (s, v) ⇒ 
                  s.orWhere(
                      Query.Where.Equals(m.child.child, v)
                    )
                }
        }
    
    def andWhere
      ( w : Query.Where )
      = this intersection where(w)

    def orWhere
      ( w : Query.Where )
      = this union where(w)

    /**
     * Mixes two selects with optimizations
     */
    def intersection
      ( s : MappingSelect )
      : MappingSelect
  }

/*
  
  assuming rock and metal are `Style` entities
  Query:
    artists.filter(
        ( "styles" includes Set(rock, metal) ) ||
        ( "styles" is Set(pop) )
      )
  
  styleSelect.primaryKey
    .subSelect()  




*/