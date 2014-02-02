{-# LANGUAGE OverloadedStrings #-}

module Fedora.Packages.SearchSpec where

import Fedora.Packages
import Fedora.Packages.Lens

import Control.Lens
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "searching Fedora Packages" $ do
    it "produces a valid result for 'firefox'" $ do
      results <- search defaultConfig (Query (SearchFilter "firefox") 100 0)
      length (results ^. srRows) > 10 `shouldBe` True
      let h = head (results ^. srRows)
      (h ^. develOwner) `shouldBe` "gecko-maint"

-- This will FAIL until we strip HTML from responses, either in the library or
-- upstream. See https://github.com/fedora-infra/fedora-packages/issues/24
--
--  describe "attempting to obtain information about a valid package" $ do
--    it "does not do anything weird at runtime" $ do
--      (Just pkg) <- packageInfo defaultConfig "firefox"
--      (pkg ^. name) `shouldBe` "firefox"

  describe "attempting to obtain information about an invalid package" $ do
    it "does not do anything weird at runtime" $ do
      result <- packageInfo defaultConfig "this_package_does_not_exist_ever"
      result `shouldBe` Nothing
