{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Tests for Fedora.Packages.Releases.
----------------------------------------------------------------------------

module Fedora.Packages.ReleasesSpec where

import Fedora.Packages
import Fedora.Packages.Lens

import Control.Lens
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "querying for releases" $ do
    it "produces a valid result for a valid package" $ do
      results <- releases defaultConfig "firefox"
      length (results ^. srRows) > 1 `shouldBe` True
      length (results ^. srRows) < 10 `shouldBe` True
      any (("Rawhide" ==) . (^. release)) (results ^. srRows) `shouldBe` True
