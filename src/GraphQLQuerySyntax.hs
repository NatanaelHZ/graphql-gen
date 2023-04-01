module GraphQLQuerySyntax where 

type Name = String

data Scalar = IntValue Integer 
            | FloatValue Double 
            | StringValue String 
            | BooleanValue Bool 
            | IDValue Name 
            deriving Show

data Value = ScalarValue Scalar
           | ListValue [Value]
           deriving Show

data Query = Query (Maybe Name) [Selection]
           deriving Show

data Selection = SingleField Name [(Name, Value)]
               | NestedField Name [(Name, Value)] [Selection]
               | InlineFragment Name [Selection]
               deriving Show

query1 = Query (Just "ArtistQuery") [SingleField "artist" [("id", ScalarValue (IntValue 1000))]]