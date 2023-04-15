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

-- data Query = Query String
-- 

-- Examples:
{- Examples generic -}
schema1 = Type "Query" 
            [
                (Field "artist" [Arg "id" (Name "ID")] (Name "Artist")),
                (Field "movie" [Arg "id" (Name "ID")] (Name "Movie"))
            ]

{- Examples Space-X API -}
-- Equivalent: type Query
spacexSchema = Type "Query"
                [
                    -- Equivalent: capsule(id: ID!): Capsule
                    --(Field "capsule" [Arg "id" (Name "ID")]  (Name "Capsule")),

                    -- capsules(find: CapsulesFind, limit: Int, offset: Int, order: String, sort: String): [Capsule]
                    (Field "capsules" [
                                Arg "find" (Name "CapsulesFind"),
                                Arg "limit" (Name "Int"),
                                Arg "offset" (Name "Int"),
                                Arg "order" (Name "String"),
                                Arg "sort" (Name "String")
                            ]
                            -- Return
                            (TypeList [(Name "Capsule")])),
                    -- company: Info
                    (Field "company" [] (Name "Info"))
                ]