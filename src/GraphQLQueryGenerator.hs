module GraphQLQueryGenerator where

import Data.List
import Debug.Trace (trace)
import Test.QuickCheck

import qualified GraphQLSchemaSyntax as S
import qualified GraphQLQuerySyntax as Q

argsData :: [(String, [(Q.Name, [Q.Value])])]
argsData = [
  ("Capsule", [("id", [Q.ScalarValue (Q.IntValue 1000),
                       Q.ScalarValue (Q.IntValue 1001)])])]

selectSchemaQuery :: [S.Query] -> Gen S.Query
selectSchemaQuery queries = elements queries

getTypeName :: S.Type -> String
getTypeName (S.TypeObject on) = on
getTypeName (S.TypeList t) = getTypeName (head t)

genField :: S.Field -> Gen Q.Selection 
genField (S.Field n _ (S.TypeObject to)) = genSelection n (S.TypeObject to)
genField (S.Field n _ _) = return (Q.SingleField n) 

genArg :: (Q.Name, [Q.Value]) -> Gen (Q.Name, Q.Value)
genArg (n, vl) = do v <- elements vl 
                    return (n, v)

genArgs :: String -> Gen [(Q.Name, Q.Value)]
genArgs on = 
  let mparams = lookup on argsData 
    in case mparams of 
         Just params -> 
           do args <- sublistOf params 
              frequency [(8, return []), 
                         (2, mapM genArg params)]
         Nothing     -> return []

genSelection :: String -> S.Type -> Gen Q.Selection
genSelection n (S.TypeObject on) = 
  let obj = lookup on S.types
    in case obj of 
         Just (S.Type _ fl) -> 
            do fields <- nub <$> listOf1 (elements fl)
               selections <- mapM genField fields
               args <- genArgs on
               return (Q.NestedField n args selections)
         Nothing -> error "Invalid object name"

generateQuery :: S.Query -> Gen Q.Query
generateQuery (S.Query n args (S.TypeObject on)) = do
  sel <- genSelection n (S.TypeObject on)
  return (Q.Query Nothing [sel])
generateQuery (S.Query n args (S.TypeList t)) = do
  if length t == 1 
    then do
      selections <- mapM (genSelection n) t
      return (Q.Query Nothing selections)
  else do
      selection <- Q.NestedField n [] <$> mapM (\t -> genSelection (getTypeName t) t) t
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