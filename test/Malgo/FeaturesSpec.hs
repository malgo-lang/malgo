{-# LANGUAGE OverloadedStrings #-}

module Malgo.FeaturesSpec (spec) where

import Control.Exception
import Data.Set (fromList)
import Effectful
import Malgo.Features
import Malgo.Prelude
import Test.Hspec

spec :: Spec
spec = describe "Features effect" $ do
  let cStyleApply = CStyleApply
      experimentalX = Experimental "X"
      flags = FeatureFlags (fromList [cStyleApply])
      flagsWithExp = FeatureFlags (fromList [cStyleApply, experimentalX])

  it "hasFeature returns True for enabled features" $ do
    result <- runEff $ runFeatures flags $ hasFeature cStyleApply
    result `shouldBe` True

  it "hasFeature returns False for disabled features" $ do
    result <- runEff $ runFeatures flags $ hasFeature experimentalX
    result `shouldBe` False

  it "getFeatureFlags returns the correct FeatureFlags" $ do
    result <- runEff $ runFeatures flagsWithExp getFeatureFlags
    result `shouldBe` flagsWithExp

  it "parseFeatures parses known features" $ do
    let features = ["c-style-apply", "experimental-X"]
        expected = FeatureFlags (fromList [CStyleApply, Experimental "X"])
    parseFeatures features `shouldBe` expected

  it "parseFeatures throws error on unknown feature" $ do
    let features = ["unknown-feature"]
    evaluate (parseFeatures features) `shouldThrow` anyErrorCall
