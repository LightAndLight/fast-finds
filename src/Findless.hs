{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Findless where

import Prelude hiding (pred)

import Bound (Scope, (>>>=))
import Bound.Scope (fromScope, transverseScope)
import Bound.Var (unvar)
import Control.DeepSeq (NFData)
import Control.Monad (ap)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (foldl', toList)
import Data.Functor.Classes (liftEq, liftShowsPrec)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

import qualified Syntax

data Expr a
  = Var a
  | MNothing
  | MJust (Expr a)
  | Maybe (Expr a) (Scope () Expr a) (Expr a)
  | Head (Expr a)
  | Filter (Scope () Expr a) (Expr a)
  | -- select field from values where field == value
    Select
      Text -- field
      (Expr a) -- values
      (Expr a) -- value
  | IfThenElse (Expr a) (Expr a) (Expr a)
  | BTrue
  | BFalse
  | Any (Vector (Expr a))
  | All (Vector (Expr a))
  | Equals (Expr a) (Expr a)
  | Project (Expr a) Text
  | List (Vector (Expr a))
  | Record (HashMap Text (Expr a))
  | Int Int
  | String Text
  deriving (Functor, Foldable, Traversable, Generic)
instance NFData a => NFData (Expr a)
deriveEq1 ''Expr
deriveShow1 ''Expr
instance Eq a => Eq (Expr a) where (==) = liftEq (==)
instance Show a => Show (Expr a) where showsPrec = liftShowsPrec showsPrec showList

instance Applicative Expr where pure = return; (<*>) = ap
instance Monad Expr where
  return = Var
  e >>= f =
    case e of
      Var a -> f a
      MNothing -> MNothing
      MJust a -> MJust (a >>= f)
      Maybe a b c -> Maybe (a >>= f) (b >>>= f) (c >>= f)
      Head a -> Head (a >>= f)
      Filter a b -> Filter (a >>>= f) (b >>= f)
      Select a b c -> Select a (b >>= f) (c >>= f)
      IfThenElse a b c -> IfThenElse (a >>= f) (b >>= f) (c >>= f)
      BTrue -> BTrue
      BFalse -> BFalse
      Any a -> Any ((>>= f) <$> a)
      All a -> All ((>>= f) <$> a)
      Equals a b -> Equals (a >>= f) (b >>= f)
      Project a b -> Project (a >>= f) b
      List a -> List ((>>= f) <$> a)
      Record a -> Record ((>>= f) <$> a)
      Int n -> Int n
      String s -> String s

directChildren :: Monad f => (forall x. Expr x -> f (Expr x)) -> Expr a -> f (Expr a)
directChildren f e =
  case e of
    Var a -> pure $ Var a
    MNothing -> pure MNothing
    MJust a -> MJust <$> f a
    Int n -> pure $ Int n
    String s -> pure $ String s
    Maybe a b c -> Maybe <$> f a <*> transverseScope f b <*> f c
    Head a -> Head <$> f a
    Filter a b -> Filter <$> transverseScope f a <*> f b
    Select a b c -> Select a <$> f b <*> f c
    IfThenElse a b c -> IfThenElse <$> f a <*> f b <*> f c
    BTrue -> pure BTrue
    BFalse -> pure BFalse
    Any a -> Any <$> traverse f a
    All a -> All <$> traverse f a
    Equals a b -> Equals <$> f a <*> f b
    Project a b -> Project <$> f a <*> pure b
    List a -> List <$> traverse f a
    Record a -> Record <$> traverse f a

mapDirectChildren :: (forall x. Expr x -> Expr x) -> Expr a -> Expr a
mapDirectChildren f = runIdentity . directChildren (Identity . f)

rewrite :: (forall x. Expr x -> Maybe (Expr x)) -> Expr a -> Expr a
rewrite f = go
 where
  go :: Expr a -> Expr a
  go e = let e' = mapDirectChildren go e in maybe e' go (f e')

type Index = Map Value (Vector (HashMap Text Value))

data Value
  = VNothing
  | VJust Value
  | VTrue
  | VFalse
  | VList (Vector Value)
  | VRecord (HashMap Text Value)
  | VIndexed (Vector Value) (Map Text Index)
  | VInt Int
  | VString Text
  deriving (Eq, Ord, Show, Generic)
instance NFData Value

buildIndexes :: Vector (HashMap Text Value) -> Map Text Index
buildIndexes content =
  foldl'
    ( \acc field ->
        Map.insert
          field
          ( Vector.foldl'
              ( \rest row -> case HashMap.lookup field row of
                  Nothing -> error "missing field"
                  Just value -> Map.insertWith (<>) value [row] rest
              )
              mempty
              content
          )
          acc
    )
    mempty
    fields
 where
  fields = toList . HashMap.keys $ Vector.head content

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
    VIndexed a' _ ->
      let b'' =
            case b of
              VIndexed b' _ -> b'
              VList b' -> b'
              _ -> error "not a list"
       in vAll $ Vector.zipWith vEquals a' b''
    VRecord items ->
      case b of
        VRecord items' ->
          if HashMap.keys items == HashMap.keys items'
            then vAll . Vector.fromList . toList $ HashMap.unionWith vEquals items items'
            else VFalse
        _ -> error "not a record"
    VList values ->
      let values'' =
            case b of
              VList values' -> values'
              VIndexed values' _ -> values'
              _ -> error "not a list"
       in vAll $ Vector.zipWith vEquals values values''
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
    VNothing ->
      case b of
        VNothing -> VTrue
        VJust{} -> VFalse
        _ -> error "not a maybe"
    VJust a' ->
      case b of
        VJust b' -> vEquals a' b'
        VNothing -> VFalse
        _ -> error "not a maybe"
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
    Int n -> VInt n
    String s -> VString s
    MNothing -> VNothing
    MJust a -> VJust $ eval ctx a
    Maybe a b c ->
      case eval ctx c of
        VNothing -> eval ctx a
        VJust c' -> eval (unvar (\() -> c') ctx) (fromScope b)
        _ -> error "not a maybe"
    Head a ->
      case eval ctx a of
        VList values ->
          maybe VNothing VJust $ values Vector.!? 0
        VIndexed values _ ->
          maybe VNothing VJust $ values Vector.!? 0
        a' -> error $ show a' <> "is not a list"
    Filter (fromScope -> pred) b ->
      case eval ctx b of
        VList values ->
          VList $
            Vector.filter
              ( \v ->
                  case eval (unvar (\() -> v) ctx) pred of
                    VTrue -> True
                    VFalse -> False
                    _ -> error "not a bool"
              )
              values
        _ -> error "not a list"
    Select field values value ->
      case eval ctx values of
        VIndexed _ index ->
          let !value' = eval ctx value
           in case Map.lookup field index of
                Nothing -> error "field missing from index"
                Just index' -> maybe (VList mempty) (VList . fmap VRecord) $ Map.lookup value' index'
        VList values' ->
          let !value' = eval ctx value
           in maybe (VList mempty) id $
                Vector.find
                  ( \case
                      VRecord items ->
                        case HashMap.lookup field items of
                          Nothing -> error "missing field"
                          Just value'' ->
                            case vEquals value' value'' of
                              VTrue -> True
                              VFalse -> False
                              _ -> error "not a bool"
                      _ -> error "not a record"
                  )
                  values'
        _ -> error "not a list"
    IfThenElse a b c ->
      case eval ctx a of
        VTrue -> eval ctx b
        VFalse -> eval ctx c
        _ -> error "not a bool"
    BTrue -> VTrue
    BFalse -> VFalse
    Any a -> vAny $ eval ctx <$> a
    All a -> vAll $ eval ctx <$> a
    Equals a b -> vEquals (eval ctx a) (eval ctx b)
    Project value field ->
      case eval ctx value of
        VRecord items ->
          case HashMap.lookup field items of
            Nothing -> error "missing field"
            Just value' -> value'
        _ -> error "not a record"
    List values -> VList $ eval ctx <$> values
    Record items -> VRecord $ eval ctx <$> items

toFindlessValue :: Syntax.Value -> Findless.Value
toFindlessValue v =
  case v of
    Syntax.VString s -> Findless.VString s
    Syntax.VInt n -> Findless.VInt n
    Syntax.VTrue -> Findless.VTrue
    Syntax.VFalse -> Findless.VFalse
    Syntax.VList vs -> Findless.VList $ toFindlessValue <$> vs
    Syntax.VRecord vs -> Findless.VRecord $ toFindlessValue <$> vs