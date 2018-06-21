{-# LANGUAGE DataKinds #-}

-- | Connection-set algebra module
module CSA where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

-- | An adjacency matrix describing connections in a directed graph
type AdjacencyMatrix = L

-- | An expression algebra that constructs connections between two elements
data Expr
  = None
  | AllToAll
  | OneToOne
  | Mask (ℝ -> ℝ -> ℝ)
  | Plus Expr Expr
  | Minus Expr Expr

-- Function: Run through matrix once
--none :: (KnownNat m, KnownNat n) => AdjacencyMatrix m n
--none = build (\_ _ -> 0)

toAdjacencyMatrix :: (KnownNat m, KnownNat n) => Expr -> AdjacencyMatrix m n
toAdjacencyMatrix expr = case expr of
  None -> build (\_ _ -> 0)
  Mask f -> build f
  _ -> build (\_ _ -> 0)
