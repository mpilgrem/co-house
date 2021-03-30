{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

{- |
Module      : Web.CoHouse.Types
Description : Types defining the UK's Companies House APIs
Copyright   : (c) Mike Pilgrem 2021
Maintainer  : public@pilgrem.com
Stability   : Experimental

This module has no connection with the UK's Companies House or its affiliates.
-}
module Web.CoHouse.Types where

import Data.Aeson ((.:), FromJSON(..), Options(fieldLabelModifier),
  genericParseJSON, defaultOptions, camelTo2, withObject)
import Data.Aeson.Types (withText)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Media.MediaType ((//))
import Servant.API (type (:>), (:<|>), Accept (..), BasicAuth,
  Capture, Get, JSON, QueryParam, ToHttpApiData (..), MimeUnrender (..))

type DayOfMonth = Int
type MonthOfYear = Int

newtype DayMonth = DayMonth (DayOfMonth, MonthOfYear)
  deriving (Eq, Show)

instance FromJSON DayMonth where
  parseJSON = withObject "DayMonth" $ \obj -> do
    day   <- obj .: "day"
    month <- obj .: "month"
    pure $ DayMonth (read day, read month)

data PDF

instance Accept PDF where
  contentType _ = "application" // "pdf"

instance MimeUnrender PDF ByteString where
  mimeUnrender _ = Right

instance MimeUnrender PDF () where
  mimeUnrender _ = const (Right ())

type CoHousePublicDataApi
  =    BasicAuth "" ()
  :>   "company"
  :>   Capture "companyNumber" Text
  :>   Get '[JSON] CompanyProfile
  :<|> BasicAuth "" ()
  :>   "search"
  :>   "companies"
  :>   QueryParam "q" Text
  :>   QueryParam "items_per_page" Int
  :>   QueryParam "start_index" Int
  :>   Get '[JSON] CompanySearchResponse
  :<|> BasicAuth "" ()
  :>   "company"
  :>   Capture "companyNumber" Text
  :>   "filing-history"
  :>   QueryParam "category" Category
  :>   QueryParam "items_per_page" Int
  :>   QueryParam "start_index" Int
  :>   Get '[JSON] FilingHistoryResponse

type CoHouseDocumentApi
  =    BasicAuth "" ()
  :>   "document"
  :>   Capture "document_id" Text
  :>   Get '[JSON] DocumentMetaDataResponse
  :<|> BasicAuth "" ()
  :>   "document"
  :>   Capture "document_id" Text
  :>   "content"
  :>   Get '[PDF] ByteString

data CompanyProfile = CompanyProfile
  { cpAccounts                             :: !(Maybe AccountsProfile)
  , cpAnnualReturn                         :: !(Maybe AnnualReturnProfile)
  , cpBranchCompanyDetails                 :: !(Maybe BranchCoProfile)
  , cpCanFile                              :: !Bool
  , cpCompanyName                          :: !Text
  , cpCompanyNumber                        :: !Text
  , cpCompanyStatus                        :: !Text
  , cpCompanyStatusDetail                  :: !(Maybe Text)
  , cpConfirmationStatement                :: !(Maybe ConfirmationStatementProfile)
  , cpDateOfCessation                      :: !(Maybe Day)
  , cpDateOfCreation                       :: !Day
  , cpEtag                                 :: !(Maybe Text)
  , cpForeignCompanyDetails                :: !(Maybe ForeignCoProfile)
  , cpHasBeenLiquidated                    :: !(Maybe Bool)
  , cpHasCharges                           :: !(Maybe Bool)
  , cpHasInsolvencyHistory                 :: !(Maybe Bool)
  , cpIsCommunityInterestCompany           :: !(Maybe Bool)
  , cpJurisdiction                         :: !Text
  , cpLastFullMembersListDate              :: !(Maybe Day)
  , cpLinks                                :: !Links
  , cpPreviousCompanyNames                 :: !(Maybe [PreviousCompanyName])
  , cpRegisteredOfficeAddress              :: !(Maybe Address)
  , cpRegisteredOfficeIsInDispute          :: !(Maybe Bool)
  , cpSicCodes                             :: !(Maybe [Text])
  , cpType                                 :: !Text
  , cpUndeliverableRegisteredOfficeAddress :: !(Maybe Bool)
  } deriving (Eq, Generic, Show)

instance FromJSON CompanyProfile where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data AccountsProfile = AccountsProfile
  { apAccountingReferenceDate :: !DayMonth
  , apLastAccounts            :: !(Maybe LastAccounts)
  , apNextDue                 :: !(Maybe Day)
  , apNextMadeUpTo            :: !Day
  , apOverdue                 :: !Bool
  } deriving (Eq, Generic, Show)

instance FromJSON AccountsProfile where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data LastAccounts = LastAccounts
  { laPeriodStartOn :: !Day
  , laMadeUpTo      :: !Day
  , laPeriodEndOn   :: !Day
  , laType          :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON LastAccounts where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data AnnualReturnProfile = AnnualReturnProfile
  { arLastMadeUpTo :: !(Maybe Day)
  , arNextDue      :: !(Maybe Day)
  , arNextMadeUpTo :: !(Maybe Day)
  , arOverdue      :: !(Maybe Bool)
  } deriving (Eq, Generic, Show)

instance FromJSON AnnualReturnProfile where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data BranchCoProfile = BranchCoProfile
  { bcBusinessActivity    :: !(Maybe Text)
  , bcParentCompanyName   :: !(Maybe Text)
  , bcParentCompanyNumber :: !(Maybe Text)
  } deriving (Eq, Generic, Show)

instance FromJSON BranchCoProfile where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data ConfirmationStatementProfile = ConfirmationStatementProfile
  { csLastMadeUpTo :: !(Maybe Day)
  , csNextDue      :: !Day
  , csNextMadeUpTo :: !Day
  , csOverdue      :: !(Maybe Bool)
  } deriving (Eq, Generic, Show)

instance FromJSON ConfirmationStatementProfile where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data ForeignCoProfile = ForeignCoProfile
  { fcAccountingRequirement       :: !(Maybe AccountingRequirement)
  , fcAccounts                    :: !(Maybe ForeignAccountsProfile)
  , fcBusinessActivity            :: !(Maybe Text)
  , fcCompanyType                 :: !(Maybe Text)
  , fcGovernedBy                  :: !(Maybe Text)
  , fcIsACreditFinanceInstitution :: !(Maybe Bool)
  , fcOriginatingRegistry         :: !(Maybe OriginatingRegistry)
  , fcRegistrationNumber          :: !(Maybe Text)
  } deriving (Eq, Generic, Show)

instance FromJSON ForeignCoProfile where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data AccountingRequirement = AccountingRequirement
  { arForeignAccountType        :: !(Maybe Text)
  , arTermsOfAccountPublication :: !(Maybe Text)
  } deriving (Eq, Generic, Show)

instance FromJSON AccountingRequirement where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data ForeignAccountsProfile = ForeignAccountsProfile
  { faAccountPeriodFrom :: !(Maybe DayMonth)
  , faAccountPeriodTo   :: !(Maybe DayMonth)
  , faMustFileWithin    :: !(Maybe Int)
  } deriving (Eq, Generic, Show)

instance FromJSON ForeignAccountsProfile where
  parseJSON = withObject "ForeignAccountsProfile" $ \obj -> do
    faAccountPeriodFrom' <- obj .: "account_period_from"
    faAccountPeriodTo'   <- obj .: "account_period_to"
    mustFileWithinObj    <- obj .: "must_file_within"
    faMustFileWithin'    <- mustFileWithinObj .: "month"
    pure $ ForeignAccountsProfile
      { faAccountPeriodFrom = faAccountPeriodFrom'
      , faAccountPeriodTo   = faAccountPeriodTo'
      , faMustFileWithin    = faMustFileWithin'
      }

data OriginatingRegistry = OriginatingRegistry
  { orCountry :: !Text
  , orName    :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON OriginatingRegistry where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data PreviousCompanyName = PreviousCompanyName
  { pcnCeasedOn      :: !Day
  , pcnEffectiveFrom :: !Day
  , pcnName          :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON PreviousCompanyName where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 3 }

data CompanySearchResponse = CompanySearchResponse
  { csrEtag         :: !(Maybe Text)
  , csrItems        :: ![CompanySearch]
  , csrItemsPerPage :: !(Maybe Int)
  , csrKind         :: !(Maybe Text)
  , csrStartIndex   :: !(Maybe Int)
  , csrTotalResults :: !(Maybe Int)
  } deriving (Eq, Generic, Show)

instance FromJSON CompanySearchResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 3 }

data CompanySearch = CompanySearch
  { csAddress               :: !Address
  , csAddressSnippet        :: !Text
  , csCompanyNumber         :: !Text
  , csCompanyStatus         :: !Text
  , csCompanyType           :: !Text
  , csDateOfCessation       :: !(Maybe Day)
  , csDateOfCreation        :: !Day
  , csDescription           :: !(Maybe Text)
  , csDescriptionIdentifier :: ![Text]
  , csKind                  :: !Text
  , csLinks                 :: !Links
  , csMatches               :: !(Maybe Matches)
  , csSnippet               :: !(Maybe Text)
  , csTitle                 :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON CompanySearch where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data Address = Address
  { aPremises      :: !(Maybe Text)
  , aAddressLine_1 :: !(Maybe Text)
  , aAddressLine_2 :: !(Maybe Text)
  , aCareOf        :: !(Maybe Text)
  , aCountry       :: !(Maybe Text)
  , aLocality      :: !(Maybe Text)
  , aPoBox         :: !(Maybe Text)
  , aPostalCode    :: !(Maybe Text)
  , aRegion        :: !(Maybe Text)
  } deriving (Eq, Generic, Show)

instance FromJSON Address where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 1 }

data Links = Links
  { lDocument         :: !(Maybe Text)
  , lDocumentMetadata :: !(Maybe Text)
  , lSelf             :: !(Maybe Text)
  } deriving (Eq, Generic, Show)

instance FromJSON Links where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 1 }

data Matches = Matches
  { mAddressSnippet :: !(Maybe [Int])
  , mSnippet        :: !(Maybe [Int])
  , mTitle          :: !(Maybe [Int])
  } deriving (Eq, Generic, Show)

instance FromJSON Matches where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 1 }

data FilingHistoryResponse = FilingHistoryResponse
  { fhrEtag                :: !(Maybe Text)
  , fhrFilingHistoryStatus :: !(Maybe Text)
  , fhrItems               :: ![FilingHistory]
  , fhrItemsPerPage        :: !Int
  , fhrKind                :: !(Maybe Text)
  , fhrStartIndex          :: !Int
  , fhrTotalCount          :: !Int
  } deriving (Eq, Generic, Show)

instance FromJSON FilingHistoryResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 3 }

data FilingHistory = FilingHistory
  { fhAnnotations       :: !(Maybe [Annotation])
  , fhAssociatedFilings :: !(Maybe [AssociatedFiling])
  , fhBarcode           :: !(Maybe Text)
  , fhCategory          :: !Category
  , fhDate              :: !Day
  , fhDescription       :: !Text
  , fhDescriptionValues :: !(Maybe DescriptionValue)
  , fhLinks             :: !(Maybe Links)
  , fhPages             :: !(Maybe Int)
  , fhPaperFiled        :: !(Maybe Bool)
  , fhResolutions       :: !(Maybe [Resolution])
  , fhSubcategory       :: !(Maybe Text)
  , fhTransactionId     :: !Text
  , fhType              :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON FilingHistory where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data Annotation = Annotation
  { aAnnotation  :: !(Maybe Text)
  , aDate        :: !Day
  , aDescription :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON Annotation where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 1 }

data AssociatedFiling = AssociatedFiling
  { afDate        :: Day
  , afDescription :: !Text
  , afType        :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON AssociatedFiling where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data Category
  = CategoryAccounts
  | CategoryAddress
  | CategoryAnnualReturn
  | CategoryCapital
  | CategoryChangeOfName
  | CategoryConfirmationStatement
  | CategoryDocumentReplacement
  | CategoryIncorporation
  | CategoryInsolvency
  | CategoryLiquidation
  | CategoryMiscellaneous
  | CategoryMortgage
  | CategoryOfficers
  | CategoryPersonsWithSignificantControl
  | CategoryResolution
  deriving (Eq, Show)

instance ToHttpApiData Category where
  toQueryParam category = case category of
    CategoryAccounts                      -> "accounts"
    CategoryAddress                       -> "address"
    CategoryAnnualReturn                  -> "annual-return"
    CategoryCapital                       -> "capital"
    CategoryChangeOfName                  -> "change-of-name"
    CategoryConfirmationStatement         -> "confirmation-statement"
    CategoryDocumentReplacement           -> "document-replacement"
    CategoryIncorporation                 -> "incorporation"
    CategoryInsolvency                    -> "insolvency"
    CategoryLiquidation                   -> "liquidation"
    CategoryMiscellaneous                 -> "miscellaneous"
    CategoryMortgage                      -> "mortgage"
    CategoryOfficers                      -> "officers"
    CategoryPersonsWithSignificantControl -> "persons-with-significant-control"
    CategoryResolution                    -> "resolution"

instance FromJSON Category where
  parseJSON = withText "Category" $ \t -> case t of
    "accounts"               -> pure CategoryAccounts
    "address"                -> pure CategoryAddress
    "annual-return"          -> pure CategoryAnnualReturn
    "capital"                -> pure CategoryCapital
    "change-of-name"         -> pure CategoryChangeOfName
    "confirmation-statement" -> pure CategoryConfirmationStatement
    "document-replacement"   -> pure CategoryDocumentReplacement
    "incorporation"          -> pure CategoryIncorporation
    "insolvency"             -> pure CategoryInsolvency
    "liquidation"            -> pure CategoryLiquidation
    "miscellaneous"          -> pure CategoryMiscellaneous
    "mortgage"               -> pure CategoryMortgage
    "officers"               -> pure CategoryOfficers
    "persons-with-significant-control" ->
      pure CategoryPersonsWithSignificantControl
    "resolution"             -> pure CategoryResolution
    _ -> fail $ "Unrecognised category type, namely: " ++ T.unpack t

data DescriptionValue = DescriptionValue
  { dvMadeUpDate :: !(Maybe Day)
  } deriving (Eq, Generic, Show)

instance FromJSON DescriptionValue where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 2 }

data Resolution = Resolution
  { rCategory    :: !Text
  , rDescription :: !Text
  , rDocumentId  :: !(Maybe Text)
  , rReceiveDate :: !(Maybe Day)
  , rSubcategory :: !Text
  , rType        :: !Text
  } deriving (Eq, Generic, Show)

instance FromJSON Resolution where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 1 }

data DocumentMetaDataResponse = DocumentMetaDataResponse
  { dmrCreatedAt :: !UTCTime
  , dmrEtag      :: !Text
  , dmrId        :: !(Maybe Text)
  , dmrLinks     :: !Links
  , dmrPages     :: !(Maybe Int)
  , dmrResources :: !(Maybe Resources)
  , dmrUpdatedAt :: !(Maybe UTCTime)
  } deriving (Eq, Generic, Show)

instance FromJSON DocumentMetaDataResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 3 }

data Resources = Resources
  { rApplicationPdf      :: !(Maybe Resource)
  , rApplicationJson     :: !(Maybe Resource)
  , rApplicationXml      :: !(Maybe Resource)
  , rApplicationXhtmlXml :: !(Maybe Resource)
  , rTextCsv             :: !(Maybe Resource)
  } deriving (Eq, Generic, Show)

instance FromJSON Resources where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = \l -> case l of
          "rApplicationXhtmlXml" -> "application/xhtml+xml"
          _ -> (camelTo2 '/' . drop 1) l
      }

data Resource = Resource
  { rContentLength :: !Int
  , rCreatedAt     :: !(Maybe UTCTime)
  , rUpdatedAt     :: !(Maybe UTCTime)
  } deriving (Eq, Generic, Show)

instance FromJSON Resource where
  parseJSON =
    genericParseJSON
      defaultOptions
      { fieldLabelModifier = camelTo2 '_' . drop 1 }
