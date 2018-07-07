{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Main where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Control.Monad.Codensity (improve)
import Control.Monad.Free (MonadFree, Free (..), liftF)
import Control.Monad.Prospect
import GHC.Generics (Generic1)
import Test.Hspec
import Test.Inspection (Result (..), inspectTest, hasNoGenerics)



main :: IO ()
main = hspec $ do
  let contT = Cont  $ const ()
      contI = ContI $ const ()

  describe "prospect" $ do
    testFree "should lazily produce values"
                 (Just "hello")
                 [ contT
                 , contT
                 ] $ do
      _ <- cont
      _ <- cont
      pure "hello"

    testFree "should not be cursed for producer patterns"
                 (Just ())
                 [ Action 1 ()
                 , Action 2 ()
                 ] $ do
      action 1
      action 2

    testFree "should not crash for a single continuation"
                 Nothing
                 [ contT
                 ] $ do
      cont

    testFree "should stop on an explicit bang pattern"
                 Nothing
                 [ contT
                 ] $ do
      !x <- cont
      pure "error"

    it "should not crash for guess" $ do
      prospect (pure @(Free Pattern) $ guess @Int)
        `shouldBe` (Nothing, [])

    it "should not crash for guess >>= pure" $ do
      prospect (pure @(Free Pattern) (guess @Int) >>= pure)
        `shouldBe` (Nothing, [])

    it "nothing you hold dear is safe" $ do
      let (u, z) = prospect $ do
            action $ guess @Int
      evaluate (force (u, z)) `shouldThrow` (== Guess)

    testFree "should stop before branching"
                 Nothing
                 [ contT
                 , Action 5 ()
                 , contT
                 ] $ do
      x <- cont
      action 5
      y <- cont
      if x
         then pure True
         else cont

  describe "verify" $ do
    it "should catch guesses" $ do
      verify guess `shouldBe` Nothing @()
      verify (guess + guess) `shouldBe` Nothing @Int
      verify (guess @[()]) `shouldBe` []

    it "should not mess with values" $ do
      verify 0 `shouldBe` Just @Int 0
      verify (1 + 2) `shouldBe` Just @Int 3

    it "should respect strictness otherwise" $ do
      verify (const 3 guess) `shouldBe` Just @Int 3


  describe "ensure" $ do
    let (a, ms) = prospect $ action 1 >> conti >>= action

    it "should crash if you're foolish" $ do
      a `shouldBe` Just ()
      evaluate (force $ show ms) `shouldThrow` (== Guess)

    it "shouldn't crash if you're wise" $ do
      a `shouldBe` Just ()
      fmap ensure ms
        `shouldBe` [Just (Action 1 ()), Just contI, Nothing]


  describe "explore" $ do
    it "should optimize away its generics" $ do
      $(inspectTest $ hasNoGenerics 'explorePattern)
        `shouldSatisfy` isSuccess


data Pattern a
  = Cont (Bool -> a)
  | ContI (Int -> a)
  | Action Int a
  deriving (Functor, Generic1)

instance Eq a => Eq (Pattern a) where
  Cont f == Cont g
      = f True  == g True
     && f False == g False
  ContI _ == ContI _ = True
  Action i a == Action j b
      = i == j
     && a == b
  _ == _ = False

instance Show (Pattern a) where
  show (Cont _)      = "Cont"
  show (ContI _)     = "ContI"
  show (Action i _ ) = "Action " ++ show i


instance NFData a => NFData (Pattern a) where
  rnf (Cont f)     = seq f ()
  rnf (ContI f)    = seq f ()
  rnf (Action i a) = seq i ()


cont :: MonadFree Pattern m => m Bool
cont = liftF $ Cont id


conti :: MonadFree Pattern m => m Int
conti = liftF $ ContI id


action :: MonadFree Pattern m => Int -> m ()
action i = liftF $ Action i ()


explorePattern :: Pattern (Free Pattern a) -> Free Pattern a
explorePattern = explore


isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False


testFree
    :: (Eq a, Show a)
    => String
    -> Maybe a
    -> [Pattern ()]
    -> (forall m. (MonadFree Pattern m) => m a)
    -> SpecWith (Arg Expectation)
testFree z v cs m =
  let (a, ms) = prospect $ improve m
   in it z $ do
        a  `shouldBe` v
        ms `shouldBe` cs

