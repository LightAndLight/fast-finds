{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bound (toScope)
import Bound.Var (Var (..))
import Control.Monad (replicateM)
import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import qualified Data.ByteString as ByteString
import qualified Data.Persist as Persist
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Findless
import qualified Optimise
import qualified Syntax
import System.Random (randomIO, randomRIO)

setup1 :: Int -> Int -> IO (Syntax.Expr Text, Vector (Vector (Text, Syntax.Value)), Text -> Syntax.Value)
setup1 ix entries = do
  xs :: Vector (Text, Int) <-
    fmap Vector.fromList . replicateM entries $
      (,)
        <$> ( do
                n :: Int <- randomRIO (1, 50)
                Text.pack <$> replicateM n randomIO
            )
          <*> randomIO
  let !needle = snd $ xs Vector.! ix
      e1 :: Syntax.Expr Text
      e1 =
        Syntax.Find
          (toScope $ Syntax.Project (Syntax.Var $ B ()) "x")
          (Syntax.Var "list")
          (toScope $ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Int needle))
          (Syntax.String "default")

      !list1 = (\(x, y) -> [("x", Syntax.VString x), ("y", Syntax.VInt y)]) <$> xs
      !list1' = Syntax.VRecord <$> list1

      !ctx1 = (\case "list" -> Syntax.VList list1'; _ -> undefined)

  pure (e1, list1, ctx1)

setup2 ::
  Int ->
  Int ->
  ( IO
      ( Syntax.Expr Text
      , Vector (Vector (Text, Syntax.Value))
      , Text -> Syntax.Value
      , Findless.Expr Text
      , Vector (Vector (Text, Findless.Value))
      , Text -> Findless.Value
      )
  )
setup2 ix entries = do
  (e1, list1, ctx1) <- setup1 ix entries
  let e2 :: Findless.Expr Text
      e2 =
        Findless.rewrite
          Optimise.rules
          (Optimise.syntaxToFindless e1)

      !list2 = (fmap . fmap) Findless.toFindlessValue <$> list1
      !index = Findless.buildIndexes list2
      !list2' = Findless.VRecord <$> list2

      ctx2 = (\case "list" -> Findless.VIndexed list2' index; _ -> undefined)
  pure (e1, list1, ctx1, e2, list2, ctx2)

mk ::
  (Syntax.Expr Text, Vector (Vector (Text, Syntax.Value))) ->
  (Findless.Expr Text, Vector (Vector (Text, Findless.Value)), Text -> Findless.Value)
mk (e1, list1) =
  let e2 :: Findless.Expr Text
      !e2 =
        Findless.rewrite
          Optimise.rules
          (Optimise.syntaxToFindless e1)

      !list2 = (fmap . fmap) Findless.toFindlessValue <$> list1
      !index = Findless.buildIndexes list2
      !list2' = Findless.VRecord <$> list2

      ctx2 = (\case "list" -> Findless.VIndexed list2' index; _ -> undefined)
   in (e2, list2, ctx2)

repeatIt :: Int -> (a -> b) -> a -> [b]
repeatIt 0 _ _ = []
repeatIt n f a =
  let !b = f a in b : repeatIt (n - 1) f a

main :: IO ()
main = do
  do
    xs :: Vector (Text, Int) <-
      fmap Vector.fromList . replicateM 10000 $
        (,)
          <$> ( do
                  n :: Int <- randomRIO (1, 50)
                  Text.pack <$> replicateM n randomIO
              )
            <*> randomIO
    let list = (\(x, y) -> [("x", Findless.VString x), ("y", Findless.VInt y)]) <$> xs
    ByteString.writeFile "10000.index" $ Persist.encode (Findless.buildIndexes list)
    ByteString.writeFile "10000.csv" $ foldMap ((<> "\n") . foldr (\(_, v) rest -> Encoding.encodeUtf8 (Findless.renderValue v) <> ", " <> rest) mempty) list
  defaultMain
    [ bgroup
        "exclude optimise and index build"
        [ env (setup2 5 10) $ \ ~(e1, _, ctx1, e2, _, ctx2) ->
            bgroup
              "10"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (Findless.eval ctx2) e2
              ]
        , env (setup2 50 100) $ \ ~(e1, _, ctx1, e2, _, ctx2) ->
            bgroup
              "100"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (Findless.eval ctx2) e2
              ]
        , env (setup2 500 1000) $ \ ~(e1, _, ctx1, e2, _, ctx2) ->
            bgroup
              "1000"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (Findless.eval ctx2) e2
              ]
        , env (setup2 5000 10000) $ \ ~(e1, _, ctx1, e2, _, ctx2) ->
            bgroup
              "10000"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (Findless.eval ctx2) e2
              ]
        ]
    , bgroup
        "include optimise and index build"
        [ env (setup1 5 10) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in Findless.eval ctx2 e2) (e1, list1)
              ]
        , env (setup1 50 100) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "100"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in Findless.eval ctx2 e2) (e1, list1)
              ]
        , env (setup1 500 1000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "1000"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in Findless.eval ctx2 e2) (e1, list1)
              ]
        , env (setup1 5000 10000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10000"
              [ bench "syntax" $ nf (Syntax.eval ctx1) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in Findless.eval ctx2 e2) (e1, list1)
              ]
        ]
    , bgroup
        "include optimise and index build but eval 10 times"
        [ env (setup1 5 10) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10"
              [ bench "syntax" $ nf (repeatIt 10 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 10 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 50 100) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "100"
              [ bench "syntax" $ nf (repeatIt 10 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 10 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 500 1000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "1000"
              [ bench "syntax" $ nf (repeatIt 10 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 10 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 5000 10000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10000"
              [ bench "syntax" $ nf (repeatIt 10 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 10 (Findless.eval ctx2) e2) (e1, list1)
              ]
        ]
    , bgroup
        "include optimise and index build but eval 100 times"
        [ env (setup1 5 10) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10"
              [ bench "syntax" $ nf (repeatIt 100 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 100 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 50 100) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "100"
              [ bench "syntax" $ nf (repeatIt 100 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 100 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 500 1000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "1000"
              [ bench "syntax" $ nf (repeatIt 100 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 100 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 5000 10000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10000"
              [ bench "syntax" $ nf (repeatIt 100 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 100 (Findless.eval ctx2) e2) (e1, list1)
              ]
        ]
    , bgroup
        "include optimise and index build but eval 1000 times"
        [ env (setup1 5 10) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10"
              [ bench "syntax" $ nf (repeatIt 1000 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 1000 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 50 100) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "100"
              [ bench "syntax" $ nf (repeatIt 1000 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 1000 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 500 1000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "1000"
              [ bench "syntax" $ nf (repeatIt 1000 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 1000 (Findless.eval ctx2) e2) (e1, list1)
              ]
        , env (setup1 5000 10000) $ \ ~(e1, list1, ctx1) ->
            bgroup
              "10000"
              [ bench "syntax" $ nf (repeatIt 1000 (Syntax.eval ctx1)) e1
              , bench "findless" $ nf (\arg -> let !(e2, _, ctx2) = mk arg in repeatIt 1000 (Findless.eval ctx2) e2) (e1, list1)
              ]
        ]
    ]
