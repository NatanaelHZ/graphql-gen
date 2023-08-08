module GraphQLQueryGenerator where

import Data.List
import Debug.Trace (trace)
import Test.QuickCheck

import qualified GraphQLSchemaSyntax as S
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

selectSchemaQuery :: [S.Query] -> Gen S.Query
selectSchemaQuery queries = elements queries

getTypeName :: S.Type -> String
getTypeName (S.TypeObject on) = on
getTypeName (S.TypeList t) = getTypeName (head t)

genField :: [S.Arg] -> S.Field -> Gen Q.Selection 
genField args (S.Field n _ (S.TypeObject to)) = genSelection n args (S.TypeObject to)
genField args (S.Field n _ _) = return (Q.SingleField n) 

isMandatory :: S.Arg -> Bool 
isMandatory (S.Arg _ b _) = b

getArgName (S.Arg n _ _) = n

genArg :: (Q.Name, [Q.Value]) -> Gen (Q.Name, Q.Value)
genArg (n, vl) = do v <- elements vl 
                    return (n, v)

genArgs :: String -> [S.Arg] -> Gen [(Q.Name, Q.Value)]
genArgs on args = 
  let mandat = map getArgName (filter isMandatory args)
      opt = map getArgName (filter (not . isMandatory) args)
      tparams = lookup on argsData 
    in case tparams of 
         Just params -> -- @TODO: check existence of arg on the argsData list
           let mparams = filter (\(n,_) -> elem n mandat) params
               oparams = filter (\(n,_) -> elem n opt) params
             in do margs <- mapM genArg mparams
                   sop   <- sublistOf oparams
                   oargs <- mapM genArg sop
                   return (margs ++ oargs)
         Nothing -> return []

genSelection :: String -> [S.Arg] -> S.Type -> Gen Q.Selection
genSelection n args (S.TypeObject on) = 
  let obj = lookup on S.types
    in case obj of 
         Just (S.Type _ fl) -> 
            do fields <- nub <$> listOf1 (elements fl)
               selections <- mapM (genField args) fields 
               args <- genArgs on args
               return (Q.NestedField n args selections)
         Nothing -> error "Invalid object name"

generateQuery :: S.Query -> Gen Q.Query
generateQuery (S.Query n args (S.TypeObject on)) = do
  sel <- genSelection n args (S.TypeObject on)
  return (Q.Query Nothing [sel])
generateQuery (S.Query n args (S.TypeList t)) = do
  if length t == 1 
    then do
      selections <- mapM (genSelection n args) t
      return (Q.Query Nothing selections)
  else do
      selection <- Q.NestedField n [] <$> mapM (\t -> genSelection (getTypeName t) args t) t
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