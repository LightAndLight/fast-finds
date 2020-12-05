{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Persist.HashMap.Strict where

import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.HashMap.Internal (HashMap (..), Leaf (L))
import Data.HashMap.Internal.Array (Array)
import qualified Data.HashMap.Internal.Array as Array
import Data.Persist (Get, Persist, Put, get, put)

instance (Persist k, Persist v) => Persist (HashMap k v) where
  get = getHashMap get get
  put = putHashMap put put

putArray :: (a -> Put ()) -> Array a -> Put ()
putArray putItem arr = do
  put len
  for_ [0 .. len -1] $ \ix ->
    putItem (Array.index arr ix)
 where
  len = Array.length arr

getArray :: Get a -> Get (Array a)
getArray getItem = do
  len <- get @Int
  Array.fromList len <$> replicateM len getItem

putHashMap :: (k -> Put ()) -> (v -> Put ()) -> HashMap k v -> Put ()
putHashMap putKey putValue = go
 where
  go hashMap =
    case hashMap of
      Empty ->
        put (0 :: Int)
      BitmapIndexed a b -> do
        put (1 :: Int)
        put a
        putArray go b
      Leaf a (L b c) -> do
        put (2 :: Int)
        put a
        putKey b
        putValue c
      Full a -> do
        put (3 :: Int)
        putArray go a
      Collision a b -> do
        put (4 :: Int)
        put a
        putArray (\(L c d) -> putKey c *> putValue d) b

getHashMap :: Get k -> Get v -> Get (HashMap k v)
getHashMap getKey getValue = go
 where
  go = do
    tag <- get @Int
    case tag of
      0 -> pure Empty
      1 -> BitmapIndexed <$> get <*> getArray go
      2 -> Leaf <$> get <*> (L <$> getKey <*> getValue)
      3 -> Full <$> getArray go
      4 -> Collision <$> get <*> getArray (L <$> getKey <*> getValue)
      _ -> fail $ "unexpected tag: " <> show tag