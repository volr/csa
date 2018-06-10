{-# LANGUAGE DataKinds #-}

-- | Tests for constructing and manipulating CSA matrices
module Main where

import qualified Numeric.LinearAlgebra.Static as LA

import CSA

--import Hedgehog
--import qualified Hedgehog.Gen as Gen
--import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hspec
--import Test.Tasty.Hedgehog

-- | Test entrypoint
main :: IO ()
main = do
    unitTree <- testSpec "CSA" unitTests
    Test.Tasty.defaultMain unitTree

unitTests :: Spec
unitTests = do
    it "Can construct an empty matrix" $
      LA.size (none :: Connection 2 3) `shouldBe` (2, 3)