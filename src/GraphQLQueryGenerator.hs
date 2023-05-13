module GraphQLQueryGenerator where

import Test.QuickCheck
import Data.List
import Debug.Trace (trace)
import qualified GraphQLSchemaSyntax as S
import qualified GraphQLQuerySyntax as Q
{-
instance Eq S.Field where
  (S.Field s1 _ _) == (S.Field s2 _ _) = s1 == s2
-}
selectSchemaQuery :: [S.Query] -> Gen S.Query
selectSchemaQuery queries = elements queries

getTypeName :: S.Type -> String
getTypeName (S.TypeObject on) = on
getTypeName (S.TypeList t) = getTypeName (head t)
{--
getSingleField :: S.Field -> Q.Selection
getSingleField (S.Field n _ (S.TypeObject to)) = genQuery n (S.TypeObject to)
getSingleField (S.Field n _ _) = Q.SingleField n
--}
genField :: S.Field -> Gen Q.Selection 
genField (S.Field n _ (S.TypeObject to)) = genQuery n (S.TypeObject to)
genField (S.Field n _ _) = return (Q.SingleField n) 

genQuery :: String -> S.Type -> Gen Q.Selection
genQuery n (S.TypeObject on) = 
  let obj = lookup on S.types
    in case obj of 
         Just (S.Type _ fl) -> 
            do fields <- nub <$> listOf1 (elements fl)
               selections <- mapM genField fields
               return (Q.NestedField n [] selections)
         Nothing -> error "Invalid object name"

generateQuery :: S.Query -> Gen Q.Query -- Name arguments type
generateQuery (S.Query n args (S.TypeObject on)) = do
  sel <- genQuery n (S.TypeObject on)
  return (Q.Query Nothing [sel])

generateQuery (S.Query n args (S.TypeList t)) = do
  if length t == 1 
    then do
      selections <- mapM (genQuery n) t
      return (Q.Query Nothing selections)
  else do
      selection <- Q.NestedField n [] <$> mapM (\t -> genQuery (getTypeName t) t) t
      return (Q.Query Nothing [selection])
-- generateQuery (S.Query n args (S.TypeList t)) = undefined

generateQuery (S.Query n args (S.TypeScalar S.PrimInt)) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField "Int"]])
generateQuery (S.Query n args (S.TypeScalar S.PrimFloat)) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField "Float"]])
generateQuery (S.Query n args (S.TypeScalar (S.NewScalar sn))) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField sn]])
-- generateQuery (S.Query n args (S.TypeScalar _)) = error "Invalid return type"

selectSchemaObjectFields :: [S.Field] -> Gen [S.Field]
selectSchemaObjectFields fields = sublistOf fields 

exportQueryGraphQL :: IO Q.Query
exportQueryGraphQL = do
  schemaQuery <- generate (selectSchemaQuery S.queries)
  let generatedQuery = generateQuery schemaQuery
  generate generatedQuery


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
>> generate $ generateQuery $ S.Query "capsules" [] (S.TypeList [(S.TypeObject "Capsule")])
>> generate $ generateQuery $ S.Query "getNumber" [] (S.TypeScalar (S.NewScalar "name_field"))
>> generate $ generateQuery $ S.Query "getNumber" [] (S.TypeScalar S.PrimInt)
>> generate $ generateQuery $ S.Query "capsuleAndDragons" [] (S.TypeList [(S.TypeObject "Capsule"), (S.TypeObject "Dragon")])
-}