{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Tests for Fedora.Packages.Bugs.
----------------------------------------------------------------------------

module Fedora.Packages.BugsSpec where

import Fedora.Packages
import Fedora.Packages.Lens

import Control.Lens
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "querying for bugs" $ do
    it "produces a valid result for a valid package" $ do
      results <- bugs defaultConfig (Query (BugsFilter "firefox" AllVersions) 10 0)
      length (results ^. srRows) > 1 `shouldBe` True
