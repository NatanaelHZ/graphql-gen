module GraphQLSchemaSyntax where

data Schema = Schema String TypeDefinition

data TypeDefinition = Scalar String 
                    | Type String [Field]
                    deriving Show 

data Field = Field String [Arg] Type 
           deriving Show 

data Arg = Arg String Type 
         deriving Show

data Type = Name String 
          | TypeList [Type]
          deriving Show


schema1 = Type "Query" 
            [
                (Field "artist" [Arg "id" (Name "ID")] (Name "Artist")),
                (Field "movie" [Arg "id" (Name "ID")] (Name "Movie"))
            ]
