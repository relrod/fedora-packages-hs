{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides functions and datatypes for searching Fedora Packages.
----------------------------------------------------------------------------

module Fedora.Packages.Search
  ( SearchFilter (..)
  , SearchQuery (..)
  , SearchResults (..)
  , packageInfo
  , search
  ) where

import Fedora.Packages.API
import Fedora.Packages.Config
import Fedora.Packages.Package

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as LS
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Text as T

data SearchFilter = SearchFilter {
    _sfSearch :: T.Text
  } deriving (Eq, Show)

data SearchQuery = SearchQuery {
    _sqSearch      :: SearchFilter
  , _sqRowsPerPage :: Int
  , _sqStartRow    :: Int
  } deriving (Eq, Show)

instance ToJSON SearchFilter where
  toJSON (SearchFilter s) = object [ "search" .= s ]

instance ToJSON SearchQuery where
  toJSON (SearchQuery s r sr) = object [ "filters" .= s
                                       , "rows_per_page" .= r
                                       , "start_row" .= sr
                                       ]

-- TODO: Can this be generalized out into a @StandardResults a@ or something?
data SearchResults = SearchResults {
    _srRows        :: [Package]
  , _srRowsPerPage :: Int
  , _srStartRow    :: Int
  , _srTotalRows   :: Int
  , _srVisibleRows :: Int
  } deriving (Eq, Show)

instance FromJSON SearchResults where
  parseJSON (Object v) = SearchResults <$>
                             v .: "rows"
                         <*> v .: "rows_per_page"
                         <*> v .: "start_row"
                         <*> v .: "total_rows"
                         <*> v .: "visible_rows"
  parseJSON _          = mzero


-- | Search Fedora Packages for a given pattern.
search :: PackagesConfig -- ^ The configuration to use.
       -> SearchQuery    -- ^ The search query to send.
       -> IO SearchResults
search c s =
  apiGet ("xapian/query/search_packages/" <> LS.toStrict (encode s)) c

-- | Search Fedora Packages and return the package if an exact one is found.
packageInfo :: PackagesConfig -> T.Text -> IO (Maybe Package)
packageInfo c q = do
  s <- search c (SearchQuery (SearchFilter q) 1 0)
  let rows = _srRows s
      matches = filter (\x -> _name x == q) rows
    in
   return $ listToMaybe matches
