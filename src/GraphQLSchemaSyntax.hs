module GraphQLSchemaSyntax where

-- Abstract syntax tree
data Schema = Schema String TypeDefinition

data TypeDefinition = Scalar String 
                    | Type String [Field]
                    deriving (Show, Eq) 

data Field = Field String [Arg] Type 
           deriving (Show, Eq) 

data Arg = Arg String Type 
         deriving (Show, Eq)

data DefaultType = PrimInt
                 | PrimFloat
                 | PrimString
                 | PrimBoolean
                 | PrimID
                 | NewScalar String -- Custom scalar type
                 deriving (Show, Eq)

data Type = TypeScalar DefaultType
          | TypeObject String
          | TypeEnum String [String]
          | TypeList [Type]
          deriving (Show, Eq)

data Query = Query String [Arg] Type
          deriving (Show, Eq)

-- Schema API
queries :: [Query]
queries = [
           -- (Query "capsule" [] (TypeObject "Capsule")), 
           (Query "capsules" [] (TypeList [(TypeObject "Capsule")])),
           (Query "company" [] (TypeObject "Info")),
           (Query "dragons" [] (TypeList [(TypeObject "Dragon")])),
           (Query "rockets" [] (TypeList [(TypeObject "Rocket")])),
           (Query "roadster" [] (TypeObject "Roadster")),
           (Query "histories" [] (TypeObject "History"))
           -- (Query "capsuleAndDragons" [] (TypeList [(TypeObject "Capsule"), (TypeObject "Capsule")]))
         ]

types :: [(String, TypeDefinition)]
types = [
          ("Capsule" , Type "Capsule" [
                                        Field "id" [] (TypeScalar PrimID), 
                                        Field "landings" [] (TypeScalar PrimInt),
                                        Field "reuse_count" [] (TypeScalar PrimInt),
                                        Field "status" [] (TypeScalar PrimString),
                                        Field "dragon" [] (TypeObject "Dragon")
                                      ]
          ),
          ("Info", Type "Info" [
                                  Field "ceo" [] (TypeScalar PrimString),
                                  Field "coo" [] (TypeScalar PrimString),
                                  Field "cto" [] (TypeScalar PrimString),
                                  Field "cto_propulsion" [] (TypeScalar PrimString)
                               ]
          ),
          ("Dragon", Type "Dragon" [
                                    Field "active" [] (TypeScalar PrimBoolean),
                                    Field "id" [] (TypeScalar PrimID),
                                    Field "name" [] (TypeScalar PrimString),
                                    Field "type" [] (TypeScalar PrimString),
                                    Field "sidewall_angle_deg" [] (TypeScalar PrimFloat)
                                   ]
          ),
          ("Rocket", Type "Rocket" [
                                    Field "active" [] (TypeScalar PrimBoolean),
                                    Field "boosters" [] (TypeScalar PrimInt),
                                    Field "company" [] (TypeScalar PrimString),
                                    Field "description" [] (TypeScalar PrimString),
                                    Field "stages" [] (TypeScalar PrimInt),
                                    Field "name" [] (TypeScalar PrimString),
                                    Field "country" [] (TypeScalar PrimString),
                                    Field "type" [] (TypeScalar PrimString)
                                   ]
          ),
          ("Roadster", Type "Roadster" [
                                  Field "details" [] (TypeScalar PrimString),
                                  Field "earth_distance_km" [] (TypeScalar PrimFloat),
                                  Field "speed_kph" [] (TypeScalar PrimFloat),
                                  Field "wikipedia" [] (TypeScalar PrimString)
                               ]
          ),
          ("History", Type "History" [
                                  Field "details" [] (TypeScalar PrimString),
                                  Field "title" [] (TypeScalar PrimString),
                                  Field "id" [] (TypeScalar PrimID),
                                  Field "links" [] (TypeObject "Link")
                               ]
          ),
          ("Link", Type "Link" [
                                  Field "article" [] (TypeScalar PrimString),
                                  Field "reddit" [] (TypeScalar PrimString),
                                  Field "wikipedia" [] (TypeScalar PrimString)
                               ]
          )
        ]

-- End Schema API















-- Examples:
{- Examples generic -}
schema1 = Type "Query" 
            [
                (Field "artist" [Arg "id" (TypeScalar PrimID)] (TypeObject "Artist")),
                (Field "movie" [Arg "id" (TypeScalar PrimID)] (TypeObject "Movie"))
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
                                Arg "limit" (TypeScalar PrimInt),
                                Arg "offset" (TypeScalar PrimInt),
                                Arg "order" (TypeScalar PrimString),
                                Arg "sort" (TypeScalar PrimString)
                            ]
                            -- Return
                            (TypeList [(TypeObject "Capsule")])),
                    -- company: Info
                    (Field "company" [] (TypeObject "Info"))
                ]