{-# LANGUAGE DataKinds #-}

{-|
Module: CSA
Description: Connectivity description using connection-set algebra
License: MIT
Maintainer: jensegholm@protonmail.com
Stability: experimental

This module uses connection-set algebra to describe connectivity between
two entities with a scalar ('Double') value. The library build on the concept
of M. Djurfeldt's Connection-Set Algebar [1].
The connection expressions can be compiled to a dependently typed
'AdjacencyMatrix' from the "Numeric.LinearAlgebra.Static" package. Such
a matrix can be used and exported as a regular n-dimensional vector.

Usage:

>>> toAdjacencyMatrix None :: L 2 2
(matrix
 [ 0.0, 0.0
 , 0.0, 0.0 ] :: L 2 2)

>>> toAdjacencyMatrix $ Minus (AllToAll 2) (OneToOne 1) :: L 2 2
(matrix
 [ 1.0, 2.0
 , 2.0, 1.0 ] :: L 2 2)

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
  = None -- ^ An empty connection (only 0s)
  | AllToAll ℝ -- ^ Full connectivity with the given value
  | OneToOne ℝ -- ^ One-to-one (diagonally) connectivity
  | Mask (ℝ -> ℝ -> ℝ) -- ^ A masked connectivity given by a function
  | Plus Expr Expr -- ^ Addition of two connectivity expressions
  | Minus Expr Expr -- ^ Subtraction of two connectivity expressions

-- | Converts an expression to an adjacency matrix by unrolling the
--   expression tree from left to right
toAdjacencyMatrix
  :: (KnownNat m, KnownNat n) => Expr -- ^ The expression to turn into a 'AdjacencyMatrix'
  -> AdjacencyMatrix m n -- ^ The resulting adjacency matrix
toAdjacencyMatrix expr = case expr of
  None -> build (\_ _ -> 0)
  Mask f -> build f
  OneToOne v -> build (\x y -> if x == y then v else 0)
  AllToAll v -> build (\_ _ -> v)
  Plus left right -> (+) (toAdjacencyMatrix left) (toAdjacencyMatrix right)
  Minus left right -> (-) (toAdjacencyMatrix left) (toAdjacencyMatrix right)
