{-# LANGUAGE DataKinds #-}

{-|
Module: CSA
Description: Connectivity description using connection-set algebra
License: MIT
Maintainer: jensegholm@protonmail.com
Stability: experimental

This module uses connection-set algebra to describe connectivity between
two entities [1]. The final product is a dependently typed 'AdjacencyMatrix'.

1: Mikael Djurfeldt. The Connection-set Algebra: a formalism for the
representation of connectivity structure in neuronal network models,
implementations in Python and C++, and their use in simulators,
BMC Neuroscience, 2011. https://doi.org/10.1186/1471-2202-12-S1-P80
-}
module CSA where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

-- | An adjacency matrix describing connections in a directed graph
type AdjacencyMatrix = L

-- | An expression algebra tree (AST) that describes connections
--   between two elements
data Expr
  = None
  | AllToAll ℝ
  | OneToOne ℝ
  | Mask (ℝ -> ℝ -> ℝ)
  | Plus Expr Expr
  | Minus Expr Expr

-- | Converts an expression to an adjacency matrix by unrolling the
--   expression tree from left to right
toAdjacencyMatrix
  :: (KnownNat m, KnownNat n) => Expr -- ^ The expression to turn into a 'AdjacencyMatrix'
  -- | The resulting adjacency matrix
  -> AdjacencyMatrix m n
toAdjacencyMatrix expr = case expr of
  None -> build (\_ _ -> 0)
  Mask f -> build f
  OneToOne v -> build (\x y -> if x == y then v else 0)
  AllToAll v -> build (\_ _ -> v)
  Plus left right -> (+) (toAdjacencyMatrix left) (toAdjacencyMatrix right)
  Minus left right -> (-) (toAdjacencyMatrix left) (toAdjacencyMatrix right)
