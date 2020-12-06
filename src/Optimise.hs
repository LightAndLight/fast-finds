{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Optimise where

import Prelude hiding (pred)

import Bound (Var (..), fromScope, instantiate1, toScope)
import Bound.Scope (hoistScope)
import Bound.Var (unvar)
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Findless
import qualified Syntax

syntaxToFindless :: Syntax.Expr a -> Findless.Expr a
syntaxToFindless expr =
  case expr of
    Syntax.Var a -> Findless.Var a
    Syntax.BTrue -> Findless.BTrue
    Syntax.BFalse -> Findless.BFalse
    Syntax.String s -> Findless.String s
    Syntax.Int n -> Findless.Int n
    Syntax.Find result value pred default_ ->
      Findless.Maybe
        (syntaxToFindless default_)
        (hoistScope syntaxToFindless result)
        (Findless.Head $ Findless.Filter (hoistScope syntaxToFindless pred) (syntaxToFindless value))
    Syntax.Any values -> Findless.Any $ syntaxToFindless <$> values
    Syntax.All values -> Findless.All $ syntaxToFindless <$> values
    Syntax.Equals a b -> Findless.Equals (syntaxToFindless a) (syntaxToFindless b)
    Syntax.Project value field -> Findless.Project (syntaxToFindless value) field
    Syntax.List values -> Findless.List $ syntaxToFindless <$> values
    Syntax.Record items -> Findless.Record $ fmap syntaxToFindless <$> items

hasSelectCondition :: Findless.Expr (Var () a) -> Bool
hasSelectCondition =
  \case
    Findless.Equals (Findless.Project (Findless.Var (B ())) _) b -> all (unvar (const False) (const True)) b
    Findless.Equals a (Findless.Project (Findless.Var (B ())) _) -> all (unvar (const False) (const True)) a
    _ -> False

extractSelectCondition :: Findless.Expr (Var () a) -> Maybe (Text, Findless.Expr a)
extractSelectCondition e =
  case e of
    Findless.Equals a b ->
      case (a, b) of
        (Findless.Project (Findless.Var (B ())) field, value) -> (,) field <$> traverse (unvar (const Nothing) Just) value
        (value, Findless.Project (Findless.Var (B ())) field) -> (,) field <$> traverse (unvar (const Nothing) Just) value
        _ -> Nothing
    _ -> Nothing

-- filter(\v -> all(p0, ..., v.x == y, ..., pn), vs)  where  v \notin free(y)
-- ~>
-- select(x, filter(\v -> p0, ..., ..., pn) vs, y)
findlessSelects :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessSelects expr =
  case expr of
    Findless.Filter pred values
      | let preds =
              ( case fromScope pred of
                  Findless.All ps -> ps
                  p -> Vector.singleton p
              )
        , let (prefix, suffix) =
                Vector.break
                  hasSelectCondition
                  preds
        , Just target <- suffix Vector.!? 0
        , Just (field, value) <- extractSelectCondition target ->
        Just $
          Findless.Select
            field
            (Findless.Filter (toScope . Findless.All $ prefix <> Vector.tail suffix) values)
            value
    _ -> Nothing

-- select(x, filter(pred, vs), y)
-- ~>
-- filter(pred, select(x, vs, y))
findlessSelectFilter :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessSelectFilter expr =
  case expr of
    Findless.Select x (Findless.Filter pred vs) y ->
      Just $ Findless.Filter pred (Findless.Select x vs y)
    _ -> Nothing

-- all() ~> true
findlessAllTrue :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessAllTrue expr =
  case expr of
    Findless.All values | Vector.null values -> Just Findless.BTrue
    _ -> Nothing

-- any() ~> false
findlessAnyFalse :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessAnyFalse expr =
  case expr of
    Findless.Any values | Vector.null values -> Just Findless.BFalse
    _ -> Nothing

findlessMaybeMaybe :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessMaybeMaybe expr =
  case expr of
    Findless.Maybe a b (Findless.Maybe c (fromScope -> d) e) ->
      Just $ Findless.Maybe (Findless.Maybe a b c) (toScope $ Findless.Maybe (F <$> a) (F <$> b) d) e
    _ -> Nothing

findlessHeadMaybe :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessHeadMaybe expr =
  case expr of
    Findless.Head (Findless.Maybe a (fromScope -> b) c) ->
      Just $ Findless.Maybe (Findless.Head a) (toScope $ Findless.Head b) c
    _ -> Nothing

findlessMaybe :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessMaybe expr =
  case expr of
    Findless.Maybe a b c ->
      case c of
        Findless.MNothing -> Just a
        Findless.MJust x -> Just $ instantiate1 x b
        _ -> Nothing
    _ -> Nothing

findlessUnfuseHeadFilterAnyEqual :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessUnfuseHeadFilterAnyEqual expr =
  case expr of
    Findless.Head (Findless.Filter (fromScope -> Findless.Any preds) values)
      | let (prefix, suffix) = Vector.break hasSelectCondition preds
        , Just target <- suffix Vector.!? 0 ->
        Just $
          Findless.Maybe
            (Findless.Head $ Findless.Filter (toScope . Findless.Any $ prefix <> Vector.tail suffix) values)
            (toScope . Findless.MJust . Findless.Var $ B ())
            (Findless.Head $ Findless.Filter (toScope target) values)
    _ -> Nothing

findlessFilterFalse :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessFilterFalse expr =
  case expr of
    Findless.Filter (fromScope -> Findless.BFalse) _ -> Just $ Findless.List []
    _ -> Nothing

findlessFilterTrue :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessFilterTrue expr =
  case expr of
    Findless.Filter (fromScope -> Findless.BTrue) a -> Just a
    _ -> Nothing

findlessHeadEmpty :: Findless.Expr a -> Maybe (Findless.Expr a)
findlessHeadEmpty expr =
  case expr of
    Findless.Head (Findless.List values) | Vector.null values -> Just Findless.MNothing
    _ -> Nothing

combine :: [a -> Maybe a] -> a -> Maybe a
combine fs a = asum $ ($ a) <$> fs

rules :: Findless.Expr a -> Maybe (Findless.Expr a)
rules =
  Optimise.combine
    [ Optimise.findlessSelects
    , Optimise.findlessSelectFilter
    , Optimise.findlessAllTrue
    , Optimise.findlessAnyFalse
    , Optimise.findlessFilterFalse
    , Optimise.findlessFilterTrue
    , -- , Optimise.findlessIfThenElse
      Optimise.findlessMaybeMaybe
    , Optimise.findlessMaybe
    , -- , Optimise.findlessMaybeIfThenElse
      Optimise.findlessUnfuseHeadFilterAnyEqual
    , Optimise.findlessHeadEmpty
    ]