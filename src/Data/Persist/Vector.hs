{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Persist.Vector where

import Data.Foldable (for_)
import Data.Persist (Persist (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector

instance Persist a => Persist (Vector a) where
  get = do
    len <- get @Int
    Vector.replicateM len get
  put vec = do
    let len = Vector.length vec
    put len
    for_ [0 .. len - 1] $ \ix -> put (Vector.unsafeIndex vec ix)