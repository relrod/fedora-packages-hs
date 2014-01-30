{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides a "Package" datatype used throughout the library.
----------------------------------------------------------------------------

module Fedora.Packages.Package
  ( Package (..)
  , Subpackage (..)
  ) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T

data Package = Package {
    _develOwner  :: T.Text
  , _icon        :: T.Text
  , _link        :: T.Text
  , _name        :: T.Text
  , _subPackages :: Maybe [Subpackage]
  , _summary     :: T.Text
  , _upstreamUrl :: T.Text
  , _description :: T.Text
  } deriving (Eq, Show)

instance FromJSON Package where
  parseJSON (Object v) = Package <$>
                             v .: "devel_owner"
                         <*> v .: "icon"
                         <*> v .: "link"
                         <*> v .: "name"
                         <*> v .: "sub_pkgs"
                         <*> v .: "summary"
                         <*> v .: "upstream_url"
                         <*> v .: "description"
  parseJSON _          = mzero

data Subpackage = Subpackage {
    _subIcon        :: T.Text
  , _subLink        :: T.Text
  , _subName        :: T.Text
  , _subSummary     :: T.Text
  , _subDescription :: T.Text
  } deriving (Eq, Show)

instance FromJSON Subpackage where
  parseJSON (Object v) = Subpackage <$>
                             v .: "icon"
                         <*> v .: "link"
                         <*> v .: "name"
                         <*> v .: "summary"
                         <*> v .: "description"
  parseJSON _          = mzero

