{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Used for configuring how to connect to the Fedora Packages API.
----------------------------------------------------------------------------

module Fedora.Packages.Config
  ( PackagesConfig (..)
  , defaultConfig
  , stagingConfig
  ) where

import Network.Http.Client

data PackagesConfig = PackagesConfig {
    _baseurl :: URL -- ^ The base URL (with protocol) of Fedora Packages.
} deriving (Eq, Show)

-- | The default configuration, which points to the production instance of
-- Fedora Packages.
defaultConfig :: PackagesConfig
defaultConfig = PackagesConfig "https://apps.fedoraproject.org"

-- | The staging configuration, which points to the staging instance of Fedora
-- Packages.
stagingConfig :: PackagesConfig
stagingConfig = PackagesConfig "https://apps.stg.fedoraproject.org"
