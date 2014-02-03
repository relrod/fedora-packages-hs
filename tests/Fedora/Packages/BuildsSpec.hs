{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Tests for Fedora.Packages.Builds.
----------------------------------------------------------------------------

module Fedora.Packages.BuildsSpec where

import Fedora.Packages
import Fedora.Packages.Lens

import Control.Lens
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "querying for builds" $ do
    it "produces a valid result for a valid package" $ do
      results <- builds defaultConfig (Query (BuildsFilter "firefox" All) 10 0)
      length (results ^. srRows) > 1 `shouldBe` True
      length (results ^. srRows) < 11 `shouldBe` True
      all (("firefox" ==) . (^. packageName)) (results ^. srRows) `shouldBe` True
