{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Executable making use of the UK's Companies House APIs.
Copyright   : (c) Mike Pilgrem 2021
Maintainer  : public@pilgrem.com
Stability   : Experimental

This module has no connection with the UK's Companies House or its affiliates.
-}
module Main (main) where

import qualified Data.List.NonEmpty as NE (last)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv)

import Data.Text (Text)
import qualified Data.Text as T (concat, pack, unpack)
import qualified Data.Text.IO as T
import Options.Applicative (Parser, (<**>), auto, command, execParser, fullDesc,
  header, help, helper, info, infoOption, long, metavar, option, progDesc,
  short, strOption, subparser, switch, value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as BL (writeFile)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (BasicAuthData(BasicAuthData))
import System.FilePath ((<.>), (</>), takeExtension, dropExtension)
import Text.URI (URI(uriPath), mkURI, unRText)

import Web.CoHouse (companyProfile, companySearch, docMetadata, docPdf,
  filingHistory)
import Web.CoHouse.Types (Address (..), Category (..), CompanyProfile (..),
  CompanySearch (..), CompanySearchResponse (..), DescriptionValue (..),
  DocumentMetaDataResponse (..), FilingHistory (..), FilingHistoryResponse (..),
  Links (..), Resources (..), LastAccounts (..), AccountsProfile (apLastAccounts))
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Applicative (optional, Alternative ((<|>)))
import Control.Monad (when)

data Options = Options
  { optApiKey  :: !(Maybe ByteString)
  , optCommand :: !Command
  } deriving (Eq, Show)

data Command
  = Search !SearchOptions
  | Profile !ProfileOptions
  | Accounts !AccountsOptions
  deriving (Eq, Show)

data SearchOptions = SearchOptions
  { soQ            :: !Text
  , soItemsPerPage :: !Int
  , soStartIndex   :: !Int
  } deriving (Eq, Show)

data ProfileOptions = ProfileOptions
  { poCoNo :: !Text
  } deriving (Eq, Show)

data AccountsOptions = AccountsOptions
  { aoCoNo       :: !Text
  , aoOutputPath :: !Text
  , aoOverwrite  :: !Bool
  } deriving (Eq, Show)

apiKeyHelp :: Text
apiKeyHelp =
  "An API key is required, either provided via environment variable " <>
  "CO_HOUSE_API_KEY or on the command line. An API key can be obtained by " <>
  "registering a user account with Companies House, creating an " <>
  "'application', and creating a new key for that application. See " <>
  "https://developer.company-information.service.gov.uk/get-started."

options :: Maybe ByteString -> Parser Options
options apiKey = Options . (<|> apiKey)
  <$> optional ( strOption
      ( long "api-key"
     <> metavar "API_KEY"
     <> help (T.unpack apiKeyHelp) ) )
  <*> subparser
      ( command "search" ( info (searchOptions <**> helper)
        ( fullDesc
       <> progDesc "Perform company search." ) )
     <> command "profile" ( info (profileOptions <**> helper)
        ( fullDesc
       <> progDesc "Obtain company profile." ) )
     <> command "accounts" ( info (accountsOptions <**> helper)
        ( fullDesc
       <> progDesc "Download accounts.") ) )

searchOptions :: Parser Command
searchOptions = Search <$> (SearchOptions
  <$> strOption
      ( long "query"
     <> short 'q'
     <> help "Search term." )
  <*> option auto
      ( long "items-per-page"
     <> short 'n'
     <> value 100
     <> help "Capped at 100 items." )
  <*> option auto
      ( long "start-index"
     <> short 'i'
     <> value 0 ))

profileOptions :: Parser Command
profileOptions = Profile <$> (ProfileOptions
  <$> strOption
      ( long "co-no"
     <> short 'c'
     <> metavar "CO_NUMBER"
     <> help "Company number." ))

accountsOptions :: Parser Command
accountsOptions = Accounts <$> (AccountsOptions
  <$> strOption
      ( long "co-no"
     <> short 'c'
     <> metavar "CO_NUMBER"
     <> help "Company number." )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "PATH"
     <> value ""
     <> help "Folder for output." )
  <*> switch
      ( long "overwrite"
     <> help "Overwrite files that already exist." ))

versionOption :: Parser (a -> a)
versionOption = infoOption "Version 0.1.0.0"
  ( long "version"
 <> help "Show version." )

main :: IO ()
main = do
  apiKey <- (B.pack <$>) <$> lookupEnv "CO_HOUSE_API_KEY"
  let opts = info (options apiKey <**> versionOption <**> helper)
        ( fullDesc
       <> progDesc "Interrogate the Companies House API"
       <> header "co-house - a tool to interrogate the Companies House API" )
  main' =<< execParser opts

main' :: Options -> IO ()
main' opts = do
  let mApiKey = optApiKey opts
  case mApiKey of
    Nothing -> do
      T.putStrLn $ "No API key specified or found. " <> apiKeyHelp
    Just apiKey -> do
      mgr <- newManager tlsManagerSettings
      let auth = BasicAuthData apiKey ""
      case optCommand opts of
        Search searchOpts -> do
          companySearch mgr auth (Just $ soQ searchOpts)
                                 (Just $ soItemsPerPage searchOpts)
                                 (Just $ soStartIndex searchOpts) >>= \case
            Right result -> do
              T.putStrLn $ "Total items available: " <> maybe "n/a" (T.pack . show)
                (csrTotalResults result)
              T.putStr (T.concat $ map display (csrItems result))
            Left err -> print err
        Profile profileOpts -> do
          companyProfile mgr auth (poCoNo profileOpts) >>= \case
            Right result -> T.putStr $ display result
            Left err -> print err
        Accounts accsOpts -> do
          let coNo       = aoCoNo accsOpts
              outputPath = aoOutputPath accsOpts
              overwrite  = aoOverwrite accsOpts
          when (outputPath /= "") $ do
            createDirectoryIfMissing True (T.unpack outputPath)
          companyProfile mgr auth coNo >>= \case
            Right cp -> do
              let coName = cpCompanyName cp
              fhs <- fetchFilingHistory mgr auth
                                        coNo
                                        (Just CategoryAccounts)
              T.putStrLn $ (T.pack . show . length) fhs <>
                " accounts found for company " <> coName <> " (" <> coNo <> ")."
              mapM_ (getPdf mgr auth outputPath coName overwrite) fhs
            Left err -> print err

class Display a where
  display :: a -> Text

instance Display CompanyProfile where
  display cp
    =  cpCompanyName cp <> " (" <> cpCompanyNumber cp <> ")\n"
    <> maybe "" display (cpRegisteredOfficeAddress cp)
    <> maybe "" display (cpAccounts cp)

instance Display Address where
  display address
    =  maybe "" (<> "\n") (aCareOf address)
    <> maybe "" (<> "\n") (aPoBox address)
    <> maybe "" (<> " ") (aPremises address)
    <> maybe "" (<> "\n") (aAddressLine_1 address)
    <> maybe "" (<> "\n") (aAddressLine_2 address)
    <> maybe "" (<> "\n") (aLocality address)
    <> maybe "" (<> "\n") (aRegion address)
    <> maybe "" (<> "\n") (aPostalCode address)
    <> maybe "" (<> "\n") (aCountry address)

instance Display AccountsProfile where
  display ap
    = maybe "" (\la -> "Last accounts: " <> display la) (apLastAccounts ap)

instance Display LastAccounts where
  display la
    =  (T.pack . show) (laMadeUpTo la) <> " (" <>
       (T.pack . show) (laPeriodStartOn la) <> " to " <>
       (T.pack . show) (laPeriodEndOn la) <> ") " <>
       laType la <> "\n"

instance Display CompanySearch where
  display cs
    =  csTitle cs <> "\n"
    <> csCompanyNumber cs <> "\n"
    <> display (csAddress cs)

instance Display FilingHistory where
  display fh
    =  fhTransactionId fh <> " "
    <> maybe "" (T.pack . show) (dvMadeUpDate =<< fhDescriptionValues fh) <> " "
    <> fromMaybe "" (lDocumentMetadata =<< fhLinks fh) <> " "
    <> (T.pack . show) (fhCategory fh) <> "\n"

fetchFilingHistory :: Manager
                    -> BasicAuthData
                    -> Text  -- ^ Company number
                    -> Maybe Category
                    -> IO [FilingHistory]
fetchFilingHistory mgr auth coNo mCat = do
  filingHistory mgr auth coNo mCat (Just 100) (Just 0) >>= \case
    Right fhr -> do
      let n = fhrTotalCount fhr
      fetchFilingHistory' mgr auth coNo mCat n
                          (fhrItems fhr)
                          100
                          1
    Left err -> error $ show err

fetchFilingHistory' :: Manager
                    -> BasicAuthData
                    -> Text  -- ^ Company number
                    -> Maybe Category
                    -> Int  -- ^ Total count
                    -> [FilingHistory]
                    -> Int  -- ^ Items per page
                    -> Int  -- ^ Page number (zero-based)
                    -> IO [FilingHistory]
fetchFilingHistory' mgr auth coNo mCat n fhs ipp p = do
  let startIndex = ipp * p
  if startIndex > n
    then pure fhs
    else do
      filingHistory mgr auth coNo mCat (Just ipp) (Just startIndex) >>= \case
        Right fhr -> do
          let n' = fhrTotalCount fhr
          if  n' /= n
            then error $ "Unexpected total count: expected " <> show n <>
              ", received " <> show n'
            else fetchFilingHistory' mgr auth coNo mCat n
                                    (fhs <> fhrItems fhr)
                                    ipp
                                    (p + 1)
        Left err -> error $ show err

documentId :: Links -> Maybe Text
documentId links = mEnd
 where
  mUri = mkURI =<< lDocumentMetadata links
  mPath = uriPath =<< mUri
  mEnd = unRText . NE.last . snd <$> mPath

getPdf :: Manager
       -> BasicAuthData
       -> Text  -- ^ Output path
       -> Text  -- ^ Company name
       -> Bool  -- ^ Allow overwriting?
       -> FilingHistory
       -> IO ()
getPdf mgr auth oPath coName overwrite fh = do
  let links = fhLinks fh
      date = (T.pack . show) $ fhDate fh
      mDocId = documentId =<< links
      mMadeUpDate = dvMadeUpDate =<< fhDescriptionValues fh
      madeUpDate = maybe "" (T.pack . show) mMadeUpDate
      fn = T.unpack oPath </> T.unpack (coName <> " - Acs " <> madeUpDate <>
        " filed " <> date) <.> "pdf"
  case mDocId of
    Just docId -> do
      docMetadata mgr auth docId >>= \case
        Right result -> do
          if isJust (rApplicationPdf =<< dmrResources result)
            then do
              docPdf mgr auth docId >>= \case
                Right bs -> do
                  fn' <- if overwrite
                          then pure fn
                          else uniqueFilePath fn
                  T.putStrLn $ "Writing file: " <> T.pack fn'
                  BL.writeFile fn' bs
                Left err' -> print err'
            else T.putStrLn "No PDF available."
        Left err -> print err
    Nothing -> T.putStrLn "No document available."

uniqueFilePath :: FilePath
               -> IO FilePath
uniqueFilePath fp = do
  fpExists <- doesFileExist fp
  if fpExists
    then uniqueFilePath' 1 fp
    else pure fp

uniqueFilePath' :: Int
                -> FilePath
                -> IO FilePath
uniqueFilePath' n fp = do
  let ext = takeExtension fp
      fp' = dropExtension fp
      fp'' = (fp' <> " - Copy (" <> show n <> ")") <.> ext
  fpExists <- doesFileExist fp''
  if fpExists
    then uniqueFilePath' (n + 1) fp
    else pure fp''
