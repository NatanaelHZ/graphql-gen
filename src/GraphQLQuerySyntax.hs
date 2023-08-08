module GraphQLQuerySyntax where 

import Data.List

type Name = String

data Scalar = IntValue Integer 
            | FloatValue Double 
            | StringValue String 
            | BooleanValue Bool 
            | IDValue Name 
            -- deriving Show

data Value = ScalarValue Scalar
           | ListValue [Value]
           -- deriving Show

data Query = Query (Maybe Name) [Selection]
           -- deriving Show

data Selection = SingleField Name -- [(Name, Value)]
               | NestedField Name [(Name, Value)] [Selection]
               -- | InlineFragment Name [Selection]
               -- deriving Show

instance Show Scalar where 
  show = exportScalar

instance Show Value where 
  show = exportValue

instance Show Selection where 
  show = exportSelection 

instance Show Query where 
  show = exportQuery
  
exportScalar :: Scalar -> String
exportScalar (IntValue x) = show x
exportScalar (FloatValue x) = show x
exportScalar (StringValue x) = x 
exportScalar (BooleanValue True) = "true"
exportScalar (BooleanValue False) = "false"

exportValue :: Value -> String 
exportValue (ScalarValue s) = exportScalar s 
exportValue (ListValue vl) = " [" ++ intercalate ",\n " (map exportValue vl) ++ "] "

exportQuery :: Query -> String
exportQuery (Query (Just n) l) = n ++ "\n{\n" ++ intercalate ",\n " (map exportSelection l) ++ "\n}\n"
exportQuery (Query Nothing l) = "\n{\n" ++ intercalate ",\n " (map exportSelection l) ++ "\n}\n"

exportSelection :: Selection -> String
exportSelection (SingleField n) = "  " ++ n
exportSelection (NestedField n [] sl) = n ++ " {\n" ++ intercalate ",\n" (map exportSelection sl) ++ "\n} "
exportSelection (NestedField n lnv sl) = n ++ 
  " (" ++ intercalate "," (map (\(n, v) -> n ++ ": " ++ "\"" ++ exportValue v ++ "\"") lnv) ++ ")"  ++
  " {\n" ++ intercalate ",\n" (map exportSelection sl) ++ "\n} "

{- 
  Example query:
  query1 = Query (Just "ArtistQuery") [
    --NestedField "artist" [("id", ScalarValue (IntValue 1000))] [SingleField "id", SingleField "name" ]
    NestedField "artist" [] [SingleField "id", SingleField "name" ]
  ]
-}