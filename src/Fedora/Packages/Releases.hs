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
  , ReleasesResult (..)
  , releases
  ) where

import Fedora.Packages.API
import Fedora.Packages.Config

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as LS
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Text as T

-- TODO: Can this be generalized out into a @StandardResults a@ or something?
data ReleasesResult = ReleasesResult {
    _rrRows        :: [Release]
  , _rrRowsPerPage :: Int
  , _rrStartRow    :: Int
  , _rrTotalRows   :: Int
  , _rrVisibleRows :: Int
  } deriving (Eq, Show)

instance FromJSON ReleasesResult where
  parseJSON (Object v) = ReleasesResult <$>
                             v .: "rows"
                         <*> v .: "rows_per_page"
                         <*> v .: "start_row"
                         <*> v .: "total_rows"
                         <*> v .: "visible_rows"
  parseJSON _          = mzero

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

data ReleasesQuery = ReleasesQuery {
    _rqFilter      :: ReleasesFilter
  , _rqRowsPerPage :: Int
  , _rqStartRow    :: Int
  } deriving (Eq, Show)

instance ToJSON ReleasesFilter where
  toJSON (ReleasesFilter s) = object [ "package" .= s ]

instance ToJSON ReleasesQuery where
  toJSON (ReleasesQuery s r sr) = object [ "filters" .= s
                                         , "rows_per_page" .= r
                                         , "start_row" .= sr
                                         ]

-- | Obtain release information about a package.
releases :: PackagesConfig   -- ^ The configuration to use.
         -> T.Text           -- ^ The name of the package to look up.
         -> IO ReleasesResult
releases c s =
  let json = ReleasesQuery (ReleasesFilter s) 1 0
  in
   apiGet ("bodhi/query/query_active_releases/" <> LS.toStrict (encode json)) c
