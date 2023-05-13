module GraphQLQueryGenerator where

import Test.QuickCheck
import Data.List
import Debug.Trace (trace)
import qualified GraphQLSchemaSyntax as S
import qualified GraphQLQuerySyntax as Q

selectSchemaQuery :: [S.Query] -> Gen S.Query
selectSchemaQuery queries = elements queries

getTypeName :: S.Type -> String
getTypeName (S.TypeObject on) = on
getTypeName (S.TypeList t) = getTypeName (head t)

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

generateQuery :: S.Query -> Gen Q.Query
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

generateQuery (S.Query n args (S.TypeScalar S.PrimInt)) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField "Int"]])
generateQuery (S.Query n args (S.TypeScalar S.PrimFloat)) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField "Float"]])
generateQuery (S.Query n args (S.TypeScalar (S.NewScalar sn))) = 
  return (Q.Query Nothing [Q.NestedField n [] [Q.SingleField sn]])

selectSchemaObjectFields :: [S.Field] -> Gen [S.Field]
selectSchemaObjectFields fields = sublistOf fields 
  
exportRandomQuery :: IO Q.Query
exportRandomQuery = do
  schemaQuery <- generate (selectSchemaQuery S.queries)
  let generatedQuery = generateQuery schemaQuery
  generate generatedQuery

{- Examples:
  Run:
  >> generate $ generateQuery $ S.Query "capsule" [] (S.TypeObject "Capsule")
  >> generate $ generateQuery $ S.Query "capsules" [] (S.TypeList [(S.TypeObject "Capsule")])
  >> generate $ generateQuery $ S.Query "getNumber" [] (S.TypeScalar (S.NewScalar "name_field"))
  >> generate $ generateQuery $ S.Query "getNumber" [] (S.TypeScalar S.PrimInt)
  >> generate $ generateQuery $ S.Query "capsuleAndDragons" [] (S.TypeList [(S.TypeObject "Capsule"), (S.TypeObject "Dragon")])
-}