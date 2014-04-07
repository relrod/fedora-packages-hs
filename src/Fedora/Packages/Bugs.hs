{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides functions and datatypes for accessing information about bugzilla
-- bugs filed for Fedora packages.
----------------------------------------------------------------------------

module Fedora.Packages.Bugs
  ( Bug (..)
  , BugsFilter (..)
  , Version (..)
  , bugs
  ) where

import Fedora.Packages.API
import Fedora.Packages.Config

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy as LS
import Data.Monoid hiding (All)
import qualified Data.Text as T

data Version =
    AllVersions
  | Rawhide
  | F20
  | F19
  | F18
  | EL7
  | EL6
  | EL5
  deriving (Eq)

instance Show Version where
  show AllVersions = ""
  show Rawhide     = "rawhide"
  show F20         = "20"
  show F19         = "19"
  show F18         = "18"
  show EL7         = "7"
  show EL6         = "6"
  show EL5         = "5"

data BugsFilter = BugsFilter {
    _bPackage :: T.Text
  , _bVersion :: Version
  } deriving (Eq, Show)

instance ToJSON BugsFilter where
  toJSON (BugsFilter p v) = object [ "package" .= p
                                   , "version" .= show v
                                   ]

data Bug = Bug {
    _bugClass        :: T.Text
  , _bugDescription  :: T.Text
  , _bugId           :: Int
  , _bugLastModified :: T.Text
  , _bugRelease      :: T.Text
  , _bugStatus       :: T.Text
  } deriving (Eq, Show)

instance FromJSON Bug where
  parseJSON (Object v) = Bug <$>
                             v .: "bug_class"
                         <*> v .: "description"
                         <*> v .: "id"
                         <*> v .: "last_modified"
                         <*> v .: "release"
                         <*> v .: "status"
  parseJSON _          = mzero

-- | Query to find bugs for a given package.
bugs :: PackagesConfig   -- ^ The configuration to use.
       -> Query BugsFilter -- ^ The search query to send.
       -> IO (StandardResults Bug)
bugs c s =
  apiGet ("bugzilla/query/query_bugs/" <> LS.toStrict (encode s)) c
