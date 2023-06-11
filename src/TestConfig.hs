module TestConfig where

import System.IO

getUrlApiGraphqlValidate :: String
getUrlApiGraphqlValidate = "https://www.itb.ec.europa.eu/graphql/api/validate"

getFilePathSchemaGraphQL :: String
getFilePathSchemaGraphQL = "./schema/schema.gql"

getGraphQLSchema :: FilePath -> IO String
getGraphQLSchema filePath = do
  content <- readFile filePath
  return content