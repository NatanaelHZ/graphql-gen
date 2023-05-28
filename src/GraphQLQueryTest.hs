-- https://www.itb.ec.europa.eu/graphql/api/docs/#/graphql/validate
-- https://joinup.ec.europa.eu/collection/interoperability-test-bed-repository/solution/graphql-validation-service/eif-perspective

{-# LANGUAGE OverloadedStrings #-}

module GraphQLQueryTest where

import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=), Value)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (hContentType)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Test (isSuccess, numTests, numShrinks, output)
import Text.Printf (printf)

import GraphQLQueryGenerator as G

paramsBodyRequest :: String -> Value
paramsBodyRequest query = object
  [ "schema" .= ("type Capsule { id: ID, landings: Int, reuse_count: Int, status: String, dragon: Dragon } type Info { ceo: String, coo: String, cto: String, cto_propulsion: String } type Dragon { active: Boolean, id: ID, name: String, type: String, sidewall_angle_deg: Float } type Rocket { active: Boolean, boosters: Int, company: String, description: String, stages: Int, name: String, country: String, type: String } type Roadster { details: String, earth_distance_km: Float, speed_kph: Float, wikipedia: String } type History { details: String, title: String, id: ID, links: Link } type Link { article: String, reddit: String, wikipedia: String } type Query { capsules: [Capsule], company: Info, dragons: [Dragon], rockets: [Rocket], roadster: Roadster, histories: History }" :: String)
  , "schemaEmbeddingMethod" .= ("STRING" :: String)
  , "query" .= query
  , "queryEmbeddingMethod" .= ("STRING" :: String)
  ]

makeHttpRequest :: String -> IO (Maybe LBS.ByteString)
makeHttpRequest query = do
  let url = "https://www.itb.ec.europa.eu/graphql/api/validate"
  initRequest <- parseRequest url
  let request = initRequest
        { method = "POST"
        , requestBody = RequestBodyLBS (Aeson.encode $ paramsBodyRequest query)
        , requestHeaders = [(hContentType, "application/json")]
        }
  manager <- newManager tlsManagerSettings
  response <- try $ httpLbs request manager
  case response of
    Left err -> do
      putStrLn $ "Failed to make the HTTP request: " ++ show (err :: SomeException)
      return Nothing
    Right resBody -> do
      let body = responseBody resBody
      return $ Just body

prop_randomQueryIsValid :: Property
prop_randomQueryIsValid = monadicIO $ do
  query <- run G.exportRandomQuery
  result <- run $ makeHttpRequest (show query)
  assert $ result == Just "[]"

main :: IO ()
main = do
  result <- quickCheckResult prop_randomQueryIsValid
  case result of
    Success { numTests = n } ->
      putStrLn $ "All " ++ show n ++ " tests passed."
    Failure { numTests = n, numShrinks = s, output = out } -> do
      putStrLn $ "Failed after " ++ show n ++ " tests with " ++ show s ++ " shrinks."
      putStrLn out


{-
main :: IO ()
main = quickCheck prop_randomQueryIsValid
-}

{-
main :: IO ()
main = do
  randomQuery <- G.exportRandomQuery
  let query = show randomQuery
  print query
  response <- makeHttpRequest query
  case response of
    Just responseBody -> do
      let body = responseBody
      putStrLn $ "Response body: " ++ show body
    Nothing -> do
      putStrLn "Failed to make the HTTP request."
-}


