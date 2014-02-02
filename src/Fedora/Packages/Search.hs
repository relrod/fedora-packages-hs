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
  , packageInfo
  , search
  ) where

import Fedora.Packages.API
import Fedora.Packages.Config
import Fedora.Packages.Package

import Data.Aeson
import qualified Data.ByteString.Lazy as LS
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Text as T

data SearchFilter = SearchFilter {
    _sfSearch :: T.Text
  } deriving (Eq, Show)

instance ToJSON SearchFilter where
  toJSON (SearchFilter s) = object [ "search" .= s ]

-- | Search Fedora Packages for a given pattern.
search :: PackagesConfig     -- ^ The configuration to use.
       -> Query SearchFilter -- ^ The search query to send.
       -> IO (StandardResults Package)
search c s =
  apiGet ("xapian/query/search_packages/" <> LS.toStrict (encode s)) c

-- | Search Fedora Packages and return the package if an exact one is found.
packageInfo :: PackagesConfig -> T.Text -> IO (Maybe Package)
packageInfo c q = do
  s <- search c (Query (SearchFilter q) 1 0)
  let rows = _srRows s
      matches = filter (\x -> _name x == q) rows
    in
   return $ listToMaybe matches
