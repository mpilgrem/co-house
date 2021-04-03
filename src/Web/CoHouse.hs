{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{- |
Module      : Web.CoHouse
Description : Bindings to the UK's Companies House APIs
Copyright   : (c) Mike Pilgrem 2021
Maintainer  : public@pilgrem.com
Stability   : Experimental

This module has no connection with the UK's Companies House or its affiliates.

The
<https://developer-specs.company-information.service.gov.uk/companies-house-public-data-api/reference Companies House Public Data API>
provides read only access to search and retrieve public company data. The
<https://developer-specs.company-information.service.gov.uk/document-api/reference Document API>
provides company filing history document metadata and downloads. This library
provides bindings in Haskell to those APIs.

See the @co-house@ console application for examples of the use of the library.
-}
module Web.CoHouse
  ( companySearch
  , companyProfile
  , companyOfficers
  , filingHistory
  , docMetadata
  , docPdf
  ) where

import Data.Maybe (isNothing)

import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Client (Manager, Request (shouldStripHeaderOnRedirect))
import Network.HTTP.Types.Header (HeaderName)
import Servant.API (BasicAuthData)
import Servant.API.Alternative ((:<|>)((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientError,
  ClientM, Scheme (Https), client, defaultMakeClientRequest, runClientM)

import Web.CoHouse.Types (Category, CoHouseDocumentApi, CoHousePublicDataApi,
  CompanyProfile, CompanySearchResponse (..), DocumentMetaDataResponse,
  FilingHistoryResponse, OfficersResponse, OrderBy, RegisterType
  )

dataApi :: Proxy CoHousePublicDataApi
dataApi = Proxy

docApi :: Proxy CoHouseDocumentApi
docApi = Proxy

data DataApi = DataApi
  { companyProfile'
      :: Text  -- ^ Company number.
      -> ClientM CompanyProfile

  , companySearch'
      :: Maybe Text  -- ^ Query.
      -> Maybe Int  -- ^ Items per page.
      -> Maybe Int  -- ^ Start index.
      -> ClientM CompanySearchResponse

  , filingHistory'
      :: Text
      -> Maybe Category
      -> Maybe Int  -- ^ Items per page.
      -> Maybe Int  -- ^ Start index.
      -> ClientM FilingHistoryResponse

  , companyOfficers'
      :: Text  -- ^ Company number.
      -> Maybe Bool -- ^ Register view.
      -> Maybe RegisterType
      -> Maybe OrderBy
      -> Maybe Int  -- ^ Items per page.
      -> Maybe Int  -- ^ Start index.
      -> ClientM OfficersResponse
  }

mkDataApi :: BasicAuthData -> DataApi
mkDataApi auth =
  let companyProfile' :<|> companySearch' :<|> filingHistory' :<|>
        companyOfficers' = client dataApi auth
  in DataApi {..}

docMetadata'
  :: BasicAuthData
  -> Text
  -> ClientM DocumentMetaDataResponse

docPdf'
  :: BasicAuthData
  -> Text
  -> ClientM ByteString

docMetadata' :<|> docPdf' = client docApi

coHousePublicDataApis :: BaseUrl
coHousePublicDataApis =
  BaseUrl Https "api.company-information.service.gov.uk" 443 ""

coHouseDocumentApis :: BaseUrl
coHouseDocumentApis =
  BaseUrl Https "document-api.company-information.service.gov.uk" 443 ""

dataApiClientEnv :: Manager -> ClientEnv
dataApiClientEnv mgr =
  ClientEnv mgr coHousePublicDataApis Nothing defaultMakeClientRequest

companyProfile
  :: Manager
  -> BasicAuthData
  -> Text  -- ^ Company number.
  -> IO (Either ClientError CompanyProfile)
companyProfile mgr auth coNo =
  runClientM
    (companyProfile' (mkDataApi auth) coNo)
    (dataApiClientEnv mgr)

companySearch
  :: Manager
  -> BasicAuthData
  -> Maybe Text  -- ^ Query.
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> IO (Either ClientError CompanySearchResponse)
companySearch mgr auth q itemsPerPage startIndex
  -- As of 2 Apr 2021, there is a bug in the Companies House API which needs to
  -- be patched. See
  -- https://forum.aws.chdev.org/t/search-companies-pagination-problem-if-items-per-page-1/3774
  | itemsPerPage == Just 1
  , isNothing startIndex || startIndex == Just 0 = do
      result <- runClientM
        (companySearch' (mkDataApi auth) q (Just 2) startIndex)
        (dataApiClientEnv mgr)
      pure $ fmap patch result
  | otherwise =
      runClientM
        (companySearch' (mkDataApi auth) q itemsPerPage startIndex)
        (dataApiClientEnv mgr)
 where
  patch csr = case csrItems csr of
    []    -> csr { csrItemsPerPage = Just 1 }
    [_]   -> csr { csrItemsPerPage = Just 1 }
    (i:_) -> csr { csrItemsPerPage = Just 1, csrItems = [i] }

filingHistory
  :: Manager
  -> BasicAuthData
  -> Text  -- ^ Company number.
  -> Maybe Category
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> IO (Either ClientError FilingHistoryResponse)
filingHistory mgr auth coNo cat itemsPerPage startIndex =
  runClientM
    (filingHistory' (mkDataApi auth) coNo cat itemsPerPage startIndex)
    (dataApiClientEnv mgr)

companyOfficers
  :: Manager
  -> BasicAuthData
  -> Text  -- ^ Company number.
  -> Maybe RegisterType
  -> Maybe OrderBy
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> IO (Either ClientError OfficersResponse)
companyOfficers mgr auth coNo mRegType mOrderBy itemsPerPage startIndex = do
  let mRV = const (Just True) =<< mRegType
  runClientM
    (companyOfficers' (mkDataApi auth) coNo mRV mRegType mOrderBy itemsPerPage startIndex)
    (dataApiClientEnv mgr)

docMetadata
  :: Manager
  -> BasicAuthData
  -> Text  -- ^ Document Id.
  -> IO (Either ClientError DocumentMetaDataResponse)
docMetadata mgr auth docId =
  runClientM
    (docMetadata' auth docId)
    (ClientEnv mgr coHouseDocumentApis Nothing defaultMakeClientRequest)

docPdf
  :: Manager
  -> BasicAuthData
  -> Text  -- ^ Document Id.
  -> IO (Either ClientError ByteString)
docPdf mgr auth docId =
  runClientM
    (docPdf' auth docId)
    (ClientEnv mgr coHouseDocumentApis Nothing mkClientRequest)
 where
  -- If the \'fetch a document\' request is successful, the response has HTTP
  -- status code 302 @Found@ with a redirection URL in the @Location@ header.
  -- The request to that URL must omit the HTTP Basic Authorisation in the
  -- original request.
  mkClientRequest b r = (defaultMakeClientRequest b r)
    { shouldStripHeaderOnRedirect = isAuthorization }
  isAuthorization :: HeaderName -> Bool
  isAuthorization "Authorization" = True
  isAuthorization _ = False
