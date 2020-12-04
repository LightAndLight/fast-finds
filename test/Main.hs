{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bound (toScope)
import Bound.Var (Var (..))
import Data.Text (Text)
import qualified Findless
import qualified Optimise
import qualified Syntax
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main =
  hspec $ do
    describe "optimise" $ do
      it "1" $ do
        let expr :: Syntax.Expr Text
            expr =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x")
                (Syntax.Var "list")
                (toScope $ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Var $ F "expected"))
                (Syntax.Var "default")
        let result =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless expr)
        result
          `shouldBe` Findless.Maybe
            (Findless.Var "default")
            (toScope $ Findless.Project (Findless.Var (B ())) "x")
            (Findless.Head $ Findless.Select "y" (Findless.Var "list") (Findless.Var "expected"))
      it "2" $ do
        let expr :: Syntax.Expr Text
            expr =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x")
                (Syntax.Var "list")
                ( toScope $
                    Syntax.All
                      [ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Var $ F "expectedY")
                      , Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "z") (Syntax.Var $ F "expectedZ")
                      ]
                )
                (Syntax.Var "default")
        let result =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless expr)
        result
          `shouldBe` Findless.Maybe
            (Findless.Var "default")
            (toScope $ Findless.Project (Findless.Var (B ())) "x")
            ( Findless.Head
                ( Findless.Select
                    "y"
                    (Findless.Select "z" (Findless.Var "list") (Findless.Var "expectedZ"))
                    (Findless.Var "expectedY")
                )
            )
      it "3" $ do
        let expr :: Syntax.Expr Text
            expr =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x")
                (Syntax.Var "list")
                ( toScope $
                    Syntax.All
                      [ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y1") (Syntax.Project (Syntax.Var $ B ()) "y2")
                      , Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "z") (Syntax.Var $ F "expectedZ")
                      ]
                )
                (Syntax.Var "default")
        let result =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless expr)
        result
          `shouldBe` Findless.Maybe
            (Findless.Var "default")
            (toScope $ Findless.Project (Findless.Var (B ())) "x")
            ( Findless.Head
                ( Findless.Filter
                    ( toScope $
                        Findless.All
                          [ Findless.Equals
                              (Findless.Project (Findless.Var (B ())) "y1")
                              (Findless.Project (Findless.Var (B ())) "y2")
                          ]
                    )
                    (Findless.Select "z" (Findless.Var "list") (Findless.Var "expectedZ"))
                )
            )
      it "4" $ do
        let expr :: Syntax.Expr Text
            expr =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x1")
                (Syntax.Var "list")
                ( toScope $
                    Syntax.All
                      [ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Var $ F "expectedY")
                      ]
                )
                ( Syntax.Find
                    (toScope $ Syntax.Project (Syntax.Var $ B ()) "x2")
                    (Syntax.Var "list")
                    ( toScope $
                        Syntax.All
                          [ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "z") (Syntax.Var $ F "expectedZ")
                          ]
                    )
                    (Syntax.Var "default")
                )
        let result =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless expr)
        print result
        () `shouldBe` ()
      it "5" $ do
        let expr :: Syntax.Expr Text
            expr =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x1")
                (Syntax.Var "list")
                ( toScope $
                    Syntax.Any
                      [ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Var $ F "expectedY")
                      , Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "z") (Syntax.Var $ F "expectedZ")
                      ]
                )
                (Syntax.Var "default")
        let result =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless expr)
        print result
        () `shouldBe` ()
    describe "eval" $ do
      it "1" $ do
        let e1 :: Syntax.Expr Text
            e1 =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x")
                (Syntax.Var "list")
                (toScope $ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Int 99))
                (Syntax.String "default")

            e2 :: Findless.Expr Text
            e2 =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless e1)

            list1 =
              [ [("x", Syntax.VString "a"), ("y", Syntax.VInt 0)]
              , [("x", Syntax.VString "b"), ("y", Syntax.VInt 2)]
              , [("x", Syntax.VString "c"), ("y", Syntax.VInt 99)]
              ]
            list2 = fmap Findless.toFindlessValue <$> list1

            result1 = Syntax.eval (\case "list" -> Syntax.VList (Syntax.VRecord <$> list1); _ -> undefined) e1
            result2 = Findless.eval (\case "list" -> Findless.VIndexed (Findless.VRecord <$> list2) (Findless.buildIndexes list2); _ -> undefined) e2

        result1 `shouldBe` Syntax.VString "c"
        Findless.toFindlessValue result1 `shouldBe` result2
      it "2" $ do
        let e1 :: Syntax.Expr Text
            e1 =
              Syntax.Find
                (toScope $ Syntax.Project (Syntax.Var $ B ()) "x")
                (Syntax.Var "list")
                (toScope $ Syntax.Equals (Syntax.Project (Syntax.Var $ B ()) "y") (Syntax.Int 200))
                (Syntax.String "default")

            e2 :: Findless.Expr Text
            e2 =
              Findless.rewrite
                Optimise.rules
                (Optimise.syntaxToFindless e1)

            list1 =
              [ [("x", Syntax.VString "a"), ("y", Syntax.VInt 0)]
              , [("x", Syntax.VString "b"), ("y", Syntax.VInt 2)]
              , [("x", Syntax.VString "c"), ("y", Syntax.VInt 99)]
              ]
            list2 = fmap Findless.toFindlessValue <$> list1

            result1 = Syntax.eval (\case "list" -> Syntax.VList (Syntax.VRecord <$> list1); _ -> undefined) e1
            result2 = Findless.eval (\case "list" -> Findless.VIndexed (Findless.VRecord <$> list2) (Findless.buildIndexes list2); _ -> undefined) e2

        result1 `shouldBe` Syntax.VString "default"
        Findless.toFindlessValue result1 `shouldBe` result2