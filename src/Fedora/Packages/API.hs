{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Used internally for actually calling out to the API. This module doesn't
-- get exposed in the cabal file.
----------------------------------------------------------------------------

module Fedora.Packages.API
  ( apiGet
  ) where

import Fedora.Packages.Config

import Data.Aeson
import qualified Data.ByteString as S
import Data.Monoid
import Network.Http.Client
import OpenSSL (withOpenSSL)

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
