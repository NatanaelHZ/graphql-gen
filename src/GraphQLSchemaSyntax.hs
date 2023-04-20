module GraphQLSchemaSyntax where

data Schema = Schema String TypeDefinition

data TypeDefinition = Scalar String 
                    | Type String [Field]
                    deriving Show 

data Field = Field String [Arg] Type 
           deriving Show 

data Arg = Arg String Type 
         deriving Show

data PrimType = PrimInt
              | PrimString
              | PrimID
              deriving Show

data Type = TypePrim PrimType
          | TypeObject String
          | TypeList [Type]
          deriving Show

data Query = Query String [Arg] Type
          deriving Show

-- 

query1 :: [Query]
query1 = [
           (Query "capsule" [] (TypeObject "Capsule")), 
           (Query "capsules" [] (TypeObject "Capsule"))
         ]
types :: [TypeDefinition]
types = [
  ("Capsule" , Type "Capsule" [])
]


-- Examples:
{- Examples generic -}
schema1 = Type "Query" 
            [
                (Field "artist" [Arg "id" (TypePrim PrimID)] (TypeObject "Artist")),
                (Field "movie" [Arg "id" (TypePrim PrimID)] (TypeObject "Movie"))
            ]

{- Examples Space-X API -}
-- Equivalent: type Query
spacexSchema = Type "Query"
                [
                    -- Equivalent: capsule(id: ID!): Capsule
                    --(Field "capsule" [Arg "id" (TypeObject "ID")]  (TypeObject "Capsule")),

                    -- capsules(find: CapsulesFind, limit: Int, offset: Int, order: String, sort: String): [Capsule]
                    (Field "capsules" [
                                Arg "find" (TypeObject "CapsulesFind"),
                                Arg "limit" (TypePrim PrimInt),
                                Arg "offset" (TypePrim PrimInt),
                                Arg "order" (TypePrim PrimString),
                                Arg "sort" (TypePrim PrimString)
                            ]
                            -- Return
                            (TypeList [(TypeObject "Capsule")])),
                    -- company: Info
                    (Field "company" [] (TypeObject "Info"))
                ]