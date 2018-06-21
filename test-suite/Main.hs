{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for constructing and manipulating CSA matrices
module Main where

import GHC.TypeLits
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

equal :: (KnownNat m, KnownNat n) => LA.L m n -> LA.L m n -> Bool
equal x y = (LA.extract x) == (LA.extract y)

unitTests :: Spec
unitTests = do
    it "Can construct an empty matrix" $
      LA.size (toAdjacencyMatrix None :: AdjacencyMatrix 2 3) `shouldBe` (2, 3)
    it "Can construct a one-to-one matrix" $
      equal (toAdjacencyMatrix (OneToOne 1) :: AdjacencyMatrix 2 2) (LA.matrix [1, 0, 0, 1] :: LA.L 2 2) `shouldBe` True
    it "Can construct a reverse one-to-one matrix" $
      equal (toAdjacencyMatrix (Minus (AllToAll 1) (OneToOne 1))) (LA.matrix [0, 1, 1, 0] :: LA.L 2 2) `shouldBe` True
