{-# LANGUAGE DataKinds #-}

-- | Connection-set algebra module
module CSA where

import GHC.TypeLits
import Numeric.LinearAlgebra.Static

-- | A connection between two types
type Connection = L

-- | A mask to apply on a connection
data Mask
  = Mask (Int -> Int -> Bool)
  | All
  | OneToOne

none :: (KnownNat m, KnownNat n) => Connection m n
none = build (\_ _ -> 0)
