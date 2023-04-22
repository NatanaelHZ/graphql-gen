module GraphQLQueryGenerator where

import Test.QuickCheck
import Data.List
import qualified GraphQLSchemaSyntax as S
import qualified GraphQLQuerySyntax as Q

instance Eq S.Field where
  (S.Field s1 _ _) == (S.Field s2 _ _) = s1 == s2

selectSchemaQuery :: [S.Query] -> Gen S.Query
selectSchemaQuery queries = elements queries

getSingleField :: S.Field -> Q.Selection 
getSingleField (S.Field n _ _) = Q.SingleField n

generateQuery :: S.Query -> Gen Q.Query -- Name arguments type
generateQuery (S.Query n args (S.TypeObject on)) = 
  let obj = lookup on S.types
    in case obj of 
         Just (S.Type _ fl) -> 
            do fields <- nub <$> listOf1 (elements fl)
               return (Q.Query Nothing [Q.NestedField n [] (map getSingleField fields)])
         Nothing -> error "Invalid object name"
generateQuery (S.Query n args (S.TypeList t)) = undefined
generateQuery (S.Query n args (S.TypeScalar (S.NewScalar sn))) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField sn]])
-- generateQuery (S.Query n args (S.TypeScalar _)) = error "Invalid return type"

selectSchemaObjectFields :: [S.Field] -> Gen [S.Field]
selectSchemaObjectFields fields = sublistOf fields 

example :: IO S.Query
example = generate (selectSchemaQuery S.queries)


{-
  1 - Escolher query: OK
  2 - Obter tipo retorno: OK
  3 - Selecionar os campos: 
  4 - Juntar tudo na query

  - Seleção dos campos sem retornar vazio
  - Ver TypeList geração
  - Estudar lista dentro de lista
-}

{-
Run:

>> generate $ generateQuery $ S.Query "capsule" [] (S.TypeObject "Capsule")

-}