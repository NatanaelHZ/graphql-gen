module GraphQLQueryData where

import qualified GraphQLQuerySyntax as Q

argsData :: [(String, [(Q.Name, [Q.Value])])]
argsData = [
  ("Capsule", [("id", [
                        Q.ScalarValue (Q.IDValue "5e9e2c5bf35918ed873b2664"),
                        Q.ScalarValue (Q.IDValue "5e9e2c5bf3591835983b2666"),
                        Q.ScalarValue (Q.IDValue "5e9e2c5bf3591882af3b2665")
                      ]
              )]
  ),
  ("Dragon", [("id", [
                        Q.ScalarValue (Q.IDValue "5e9d058759b1ff74a7ad5f8f"),
                        Q.ScalarValue (Q.IDValue "5e9d058859b1ffd8e2ad5f90")
                      ]
              )]
  ),
  ("Rocket", [("id", [
                        Q.ScalarValue (Q.IDValue "5e9d0d95eda69955f709d1eb"),
                        Q.ScalarValue (Q.IDValue "5e9d0d95eda69973a809d1ec"),
                        Q.ScalarValue (Q.IDValue "5e9d0d95eda69974db09d1ed")
                      ]
              )]
  )
  ]
