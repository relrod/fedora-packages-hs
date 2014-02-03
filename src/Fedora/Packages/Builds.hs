{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides functions and datatypes for accessing information about Fedora
-- package builds.
----------------------------------------------------------------------------

module Fedora.Packages.Builds
  ( Build (..)
  , BuildsFilter (..)
  , BuildState (..)
  , CompletionTimeDisplay (..)
  , builds
  ) where

import Fedora.Packages.API
import Fedora.Packages.Config

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy as LS
import Data.Monoid hiding (All)
import qualified Data.Text as T

data BuildState =
    All
  | Building
  | Success
  | Failed
  | Cancelled
  | Deleted
  deriving (Eq, Show)

buildStateToText :: BuildState -> T.Text
buildStateToText All       = ""
buildStateToText Building  = "0"
buildStateToText Success   = "1"
buildStateToText Deleted   = "2"
buildStateToText Failed    = "3"
buildStateToText Cancelled = "4"

data BuildsFilter = BuildsFilter {
    _bfPackage :: T.Text
  , _bfState   :: BuildState
  } deriving (Eq, Show)

instance ToJSON BuildsFilter where
  toJSON (BuildsFilter p s) = object [ "package" .= p
                                     , "state"   .= buildStateToText s
                                     ]

data CompletionTimeDisplay = CompletionTimeDisplay {
    _elapsed :: T.Text
  , _time    :: T.Text
  , _when    :: T.Text
  } deriving (Eq, Show)

instance FromJSON CompletionTimeDisplay where
  parseJSON (Object v) = CompletionTimeDisplay <$>
                             v .:  "elapsed"
                         <*> v .:  "time"
                         <*> v .:  "when"
  parseJSON _          = mzero

data Build = Build {
    _buildId               :: Int
  , _completionTime        :: T.Text
  , _completionTimeDisplay :: CompletionTimeDisplay
  , _completionTs          :: Double
  , _creationEventId       :: Int
  , _creationTime          :: T.Text
  , _creationTs            :: Double
  , _epoch                 :: Maybe Double
  , _name'                 :: T.Text
  , _nvr                   :: T.Text
  , _ownerId               :: Int
  , _ownerName             :: T.Text
  , _packageId             :: Int
  , _packageName           :: T.Text
  , _packageRelease        :: T.Text
  , _state                 :: Int
  , _stateStr              :: T.Text
  , _taskId                :: Int
  , _version               :: T.Text
  , _volumeId              :: Int
  , _volumeName            :: T.Text
  } deriving (Eq, Show)

instance FromJSON Build where
  parseJSON (Object v) = Build <$>
                             v .:  "build_id"
                         <*> v .:  "completion_time"
                         <*> v .:  "completion_time_display"
                         <*> v .:  "completion_ts"
                         <*> v .:  "creation_event_id"
                         <*> v .:  "creation_time"
                         <*> v .:  "creation_ts"
                         <*> v .:? "epoch"
                         <*> v .:  "name"
                         <*> v .:  "nvr"
                         <*> v .:  "owner_id"
                         <*> v .:  "owner_name"
                         <*> v .:  "package_id"
                         <*> v .:  "package_name"
                         <*> v .:  "release"
                         <*> v .:  "state"
                         <*> v .:  "state_str"
                         <*> v .:  "task_id"
                         <*> v .:  "version"
                         <*> v .:  "volume_id"
                         <*> v .:  "volume_name"
  parseJSON _          = mzero

-- | Query to find builds for a given package.
builds :: PackagesConfig     -- ^ The configuration to use.
       -> Query BuildsFilter -- ^ The search query to send.
       -> IO (StandardResults Build)
builds c s =
  apiGet ("koji/query/query_builds/" <> LS.toStrict (encode s)) c
