{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

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
  CompanyProfile, CompanySearchResponse, DocumentMetaDataResponse, FilingHistoryResponse, OfficersResponse, OrderBy, RegisterType
  )

dataApi :: Proxy CoHousePublicDataApi
dataApi = Proxy

docApi :: Proxy CoHouseDocumentApi
docApi = Proxy

companyProfile'
  :: BasicAuthData
  -> Text  -- ^ Company number.
  -> ClientM CompanyProfile

companySearch'
  :: BasicAuthData
  -> Maybe Text  -- ^ Query.
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> ClientM CompanySearchResponse

filingHistory'
  :: BasicAuthData
  -> Text
  -> Maybe Category
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> ClientM FilingHistoryResponse

companyOfficers'
  :: BasicAuthData
  -> Text  -- ^ Company number.
  -> Maybe Bool -- ^ Register view.
  -> Maybe RegisterType
  -> Maybe OrderBy
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> ClientM OfficersResponse

docMetadata'
  :: BasicAuthData
  -> Text
  -> ClientM DocumentMetaDataResponse

docPdf'
  :: BasicAuthData
  -> Text
  -> ClientM ByteString

companyProfile' :<|> companySearch' :<|> filingHistory' :<|> companyOfficers' =
  client dataApi

docMetadata' :<|> docPdf' = client docApi

coHousePublicDataApis :: BaseUrl
coHousePublicDataApis =
  BaseUrl Https "api.company-information.service.gov.uk" 443 ""

coHouseDocumentApis :: BaseUrl
coHouseDocumentApis =
  BaseUrl Https "document-api.company-information.service.gov.uk" 443 ""

companyProfile
  :: Manager
  -> BasicAuthData
  -> Text  -- ^ Company number.
  -> IO (Either ClientError CompanyProfile)
companyProfile mgr auth coNo =
  runClientM
    (companyProfile' auth coNo)
    (ClientEnv mgr coHousePublicDataApis Nothing defaultMakeClientRequest)

companySearch
  :: Manager
  -> BasicAuthData
  -> Maybe Text  -- ^ Query.
  -> Maybe Int  -- ^ Items per page.
  -> Maybe Int  -- ^ Start index.
  -> IO (Either ClientError CompanySearchResponse)
companySearch mgr auth q itemsPerPage startIndex =
  runClientM
    (companySearch' auth q itemsPerPage startIndex)
    (ClientEnv mgr coHousePublicDataApis Nothing defaultMakeClientRequest)

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
    (filingHistory' auth coNo cat itemsPerPage startIndex)
    (ClientEnv mgr coHousePublicDataApis Nothing defaultMakeClientRequest)

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
    (companyOfficers' auth coNo mRV mRegType mOrderBy itemsPerPage startIndex)
    (ClientEnv mgr coHousePublicDataApis Nothing defaultMakeClientRequest)

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
