{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax where

import Prelude hiding (pred)

import Bound (Scope, fromScope, (>>>=))
import Bound.Var (unvar)
import Control.DeepSeq (NFData)
import Control.Monad (ap)
import Data.Foldable (Foldable (toList))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

data Expr a
  = Var a
  | -- find result in value
    -- where pred
    -- otherwise default
    Find
      (Scope () Expr a) -- result
      (Expr a) -- value to search
      (Scope () Expr a) -- predicate
      (Expr a) -- default
  | BTrue
  | BFalse
  | Any (Vector (Expr a))
  | All (Vector (Expr a))
  | Equals (Expr a) (Expr a)
  | Project (Expr a) Text
  | List (Vector (Expr a))
  | Record (Vector (Text, Expr a))
  | String Text
  | Int Int
  deriving (Functor, Generic)
instance NFData a => NFData (Expr a)

instance Applicative Expr where pure = return; (<*>) = ap
instance Monad Expr where
  return = Var
  e >>= f =
    case e of
      Var a -> f a
      BTrue -> BTrue
      BFalse -> BFalse
      Find a b c d -> Find (a >>>= f) (b >>= f) (c >>>= f) (d >>= f)
      Any a -> Any ((>>= f) <$> a)
      All a -> All ((>>= f) <$> a)
      Equals a b -> Equals (a >>= f) (b >>= f)
      Project a b -> Project (a >>= f) b
      List a -> List ((>>= f) <$> a)
      Record a -> Record (fmap (>>= f) <$> a)
      String s -> String s
      Int n -> Int n

data Value
  = VRecord (Vector (Text, Value))
  | VList (Vector Value)
  | VTrue
  | VFalse
  | VInt Int
  | VString Text
  deriving (Eq, Ord, Show, Generic)
instance NFData Value

vAll :: Vector Value -> Value
vAll =
  Vector.foldl'
    (\a b -> case a of VFalse -> VFalse; VTrue -> b; _ -> error "not a bool")
    VTrue

vAny :: Vector Value -> Value
vAny =
  Vector.foldl'
    (\a b -> case a of VTrue -> VTrue; VFalse -> b; _ -> error "not a bool")
    VFalse

vEquals :: Value -> Value -> Value
vEquals a b =
  case a of
    VRecord items ->
      case b of
        VRecord items' ->
          if fmap fst items == fmap fst items'
            then
              vAll . Vector.fromList . toList $
                Vector.zipWith (\(_, c) (_, d) -> vEquals c d) items items'
            else VFalse
        _ -> error "not a record"
    VList values ->
      case b of
        VList values' -> vAll $ Vector.zipWith vEquals values values'
        _ -> error "not a list"
    VTrue ->
      case b of
        VTrue -> VTrue
        VFalse -> VFalse
        _ -> error "not a bool"
    VFalse ->
      case b of
        VFalse -> VTrue
        VTrue -> VFalse
        _ -> error "not a bool"
    VInt n ->
      case b of
        VInt n' ->
          if n == n' then VTrue else VFalse
        _ -> error "not an int"
    VString s ->
      case b of
        VString s' ->
          if s == s' then VTrue else VFalse
        _ -> error "not an int"

eval :: (a -> Value) -> Expr a -> Value
eval ctx expr =
  case expr of
    Var a -> ctx a
    BTrue -> VTrue
    BFalse -> VFalse
    Int n -> VInt n
    String s -> VString s
    Find result value pred default_ ->
      case eval ctx value of
        VList values ->
          case Vector.find
            ( \v -> case eval (unvar (\() -> v) ctx) (fromScope pred) of
                VTrue -> True
                VFalse -> False
                _ -> error "not a bool"
            )
            values of
            Nothing -> eval ctx default_
            Just value' -> eval (unvar (\() -> value') ctx) (fromScope result)
        _ -> error "not a list"
    Any values ->
      vAny $ eval ctx <$> values
    All values ->
      vAll $ eval ctx <$> values
    Equals a b ->
      let !a' = eval ctx a
          !b' = eval ctx b
       in vEquals a' b'
    Project value field ->
      case eval ctx value of
        VRecord items ->
          case Vector.find ((field ==) . fst) items of
            Nothing -> error "missing field"
            Just (_, value') -> value'
        _ -> error "not a record"
    List values -> VList $ eval ctx <$> values
    Record items -> VRecord $ fmap (eval ctx) <$> items