{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides lenses for data types found within Fedora.Packages.
----------------------------------------------------------------------------

module Fedora.Packages.Lens where

import Control.Lens (makeLenses)

import Fedora.Packages.API
import Fedora.Packages.Config
import Fedora.Packages.Package
import Fedora.Packages.Releases
import Fedora.Packages.Search

makeLenses ''PackagesConfig
makeLenses ''Package
makeLenses ''Release
makeLenses ''SearchFilter
makeLenses ''StandardResults
makeLenses ''Query
