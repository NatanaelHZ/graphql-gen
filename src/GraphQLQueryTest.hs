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
import TestConfig as Config

paramsBodyRequest :: String -> String -> Value
paramsBodyRequest query schema = object
  [ "schema" .= schema
  , "schemaEmbeddingMethod" .= ("STRING" :: String)
  , "query" .= query
  , "queryEmbeddingMethod" .= ("STRING" :: String)
  ]

makeHttpRequest :: String -> String -> IO (Maybe LBS.ByteString)
makeHttpRequest query schema = do
  let url = Config.getUrlApiGraphqlValidate
  initRequest <- parseRequest url
  let request = initRequest
        { method = "POST"
        , requestBody = RequestBodyLBS (Aeson.encode $ paramsBodyRequest query schema)
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
  schema <- run $ Config.getGraphQLSchema Config.getFilePathSchemaGraphQL
  result <- run $ makeHttpRequest (show query) schema
  assert $ result == Just "[]"

main :: IO ()
main = do
  result <- quickCheckWithResult stdArgs { chatty = False } prop_randomQueryIsValid
  putStrLn $ "Result: " ++ show (isSuccess result)
  putStrLn $ output result
