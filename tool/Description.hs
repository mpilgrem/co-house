{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Executable to generate code for module
              Web.CoHouse.Types.Description
Copyright   : (c) Mike Pilgrem 2021
Maintainer  : public@pilgrem.com
Stability   : Experimental

This module has no connection with the UK's Companies House or its affiliates.
-}
module Main (main) where

import Data.Char (toUpper)
import Data.List (sort)

import qualified Data.HashMap.Strict as HM ((!?), keys)
import Data.Text (Text)
import qualified Data.Text as T (concat, cons, intercalate, splitOn, uncons)
import qualified Data.Text.IO as T (putStr)
import Data.Yaml
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as B

descriptionUrl :: String
descriptionUrl = "https://raw.githubusercontent.com/companieshouse/api-enumerations/master/filing_history_descriptions.yml"

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  request <- parseRequest descriptionUrl
  response <- httpLbs request mgr
  let bs = responseBody response
      result = decodeEither' (B.toStrict bs) :: Either ParseException Value
  case result of
    Right yml -> T.putStr $ code yml
    Left err -> print err

code :: Value -> Text
code yml =
  "-- This file was generated using tool-description\n" <>
  "\n" <>
  "{-# LANGUAGE OverloadedStrings #-}\n" <>
  "\n" <>
  "{- |\n" <>
  "Module      : Web.CoHouse.Types.Description\n" <>
  "Description : Types defining the UK's Companies House APIs\n" <>
  "Copyright   : (c) Mike Pilgrem 2021\n" <>
  "Maintainer  : public@pilgrem.com\n" <>
  "Stability   : Experimental\n" <>
  "\n" <>
  "This module has no connection with the UK's Companies House or its affiliates.\n" <>
  "-}\n" <>
  "module Web.CoHouse.Types.Description\n" <>
  "  ( Description (..)\n" <>
  "  , isAccounts\n" <>
  "  ) where\n" <>
  "\n" <>
  "import Data.Aeson (FromJSON (..), withText)\n" <>
  "import qualified Data.Text as T (unpack)\n" <>
  "\n" <>
  "data Description\n" <>
  "  = " <> ymlToEnum <> "\n" <>
  "  deriving (Eq, Show)\n" <>
  "\n" <>
  "instance FromJSON Description where\n" <>
  "  parseJSON = withText \"Description\" $ \\t -> case t of\n" <>
  ymlToParseJSON <>
  "    desc -> error $ \"Unknown description: \" <> T.unpack desc\n" <>
  "\n" <>
  "isAccounts :: Description -> Bool\n" <>
  "isAccounts desc = case desc of\n" <>
  ymlToIsAccounts <>
  "  _ -> False\n"
 where
  ds = HM.keys $ case yml of
    Object obj -> case obj HM.!? "description" of
      Just (Object obj') -> obj'
      _ -> error "No expected 'description' object."
    _ -> error "No expected 'description' object."
  -- Add "legacy" because omitted from filing_history_descriptions.yml
  ds' = sort $ "legacy" : ds
  ds'' = map kebabToList ds'
  ds''' = map toPascalCase ds''
  accounts = filter (\p -> head p == "accounts") ds''
  ymlToEnum = T.intercalate "\n  | " ds'''
  ymlToIsAccounts = T.concat $ map (ymlToIsAccounts' . toPascalCase) accounts
  ymlToIsAccounts' t = "  " <> t <> " -> True\n"
  ymlToParseJSON = T.concat $ zipWith ymlToParseJSON' ds' ds'''
  ymlToParseJSON' t1 t2 = "    \"" <> t1 <> "\" -> pure " <> t2 <> "\n"

toPascalCase :: [Text] -> Text
toPascalCase ts =
  let ucs = map (tu . T.uncons) ts
      tu = maybe "" (\(c, t') -> T.cons (toUpper c) t')
  in T.concat ucs

kebabToList :: Text -> [Text]
kebabToList = T.splitOn "-"
