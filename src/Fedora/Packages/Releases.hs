{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides functions and datatypes for getting release information about Fedora
-- packages.
----------------------------------------------------------------------------

module Fedora.Packages.Releases
  ( Release (..)
  , releases
  ) where

import Fedora.Packages.API
import Fedora.Packages.Config

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as LS
import Data.Monoid
import qualified Data.Text as T

data Release = Release {
    _release        :: T.Text
  , _stableVersion  :: T.Text
  , _testingVersion :: T.Text
  } deriving (Eq, Show)

instance FromJSON Release where
  parseJSON (Object v) = Release <$>
                             v .: "release"
                         <*> v .: "stable_version"
                         <*> v .: "testing_version"
  parseJSON _          = mzero

data ReleasesFilter = ReleasesFilter {
    _rfPackage :: T.Text
  } deriving (Eq, Show)

instance ToJSON ReleasesFilter where
  toJSON (ReleasesFilter s) = object [ "package" .= s ]

-- | Obtain release information about a package.
releases :: PackagesConfig   -- ^ The configuration to use.
         -> T.Text           -- ^ The name of the package to look up.
         -> IO (StandardResults Release)
releases c s =
  let rJson = Query (ReleasesFilter s) 1 0
  in
   apiGet ("bodhi/query/query_active_releases/" <> LS.toStrict (encode rJson)) c
