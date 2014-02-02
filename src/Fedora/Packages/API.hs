{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Functions and datatypes for directly calling out to the API and handling
-- queries to and responses from it.
----------------------------------------------------------------------------

module Fedora.Packages.API
  ( -- * Responses
    StandardResults (..)
    -- * Requests
  , Query (..)
  , apiGet
  ) where

import Fedora.Packages.Config

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as S
import Data.Monoid
import Network.Http.Client
import OpenSSL (withOpenSSL)

----------------------------------------------------------------------------
-- Responses
----------------------------------------------------------------------------

data StandardResults a = StandardResults {
    _srRows        :: [a]
  , _srRowsPerPage :: Int
  , _srStartRow    :: Int
  , _srTotalRows   :: Int
  , _srVisibleRows :: Int
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (StandardResults a) where
  parseJSON (Object v) = StandardResults <$>
                             v .: "rows"
                         <*> v .: "rows_per_page"
                         <*> v .: "start_row"
                         <*> v .: "total_rows"
                         <*> v .: "visible_rows"
  parseJSON _          = mzero

----------------------------------------------------------------------------
-- Requests
----------------------------------------------------------------------------

-- | Sets things that are common to all requests that we make.
prepareRequest :: Method -> S.ByteString -> RequestBuilder ()
prepareRequest m url = do
  http m url
  setAccept "application/json"
  setContentType "application/json"

finishRequest :: FromJSON a => Connection -> IO a
finishRequest cnx = do
  x <- receiveResponse cnx jsonHandler
  closeConnection cnx
  return x

-- | Perform a GET request to the API.
apiGet :: FromJSON a => S.ByteString -> PackagesConfig -> IO a
apiGet url c = withOpenSSL $ do
  cnx <- establishConnection (_baseurl c)
  q <- buildRequest $ prepareRequest GET ("/packages/fcomm_connector/" <> url)
  sendRequest cnx q emptyBody
  finishRequest cnx


data Query a = Query {
    _qSearch      :: a
  , _qRowsPerPage :: Int
  , _qStartRow    :: Int
  } deriving (Eq, Show)

instance ToJSON a => ToJSON (Query a) where
  toJSON (Query s r sr) = object [ "filters" .= s
                                 , "rows_per_page" .= r
                                 , "start_row" .= sr
                                 ]
