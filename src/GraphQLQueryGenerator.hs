module GraphQLQueryGenerator where

import Test.QuickCheck
import GraphQLSchemaSyntax

generateQuery :: [Query] -> Gen Query
generateQuery queries = oneof (map return queries)

example :: IO Query
example = generate (generateQuery query1)
