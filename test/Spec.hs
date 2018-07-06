{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

module Main where

import Control.Applicative
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Control.Monad (guard)
import Control.Monad.Accursed
import Control.Monad.Codensity (improve)
import Control.Monad.Free
import GHC.Generics
import Test.Hspec
import Test.Inspection



main :: IO ()
main = hspec $ do
  let contT = Cont $ const ()
  describe "Free" $ do
    testFree "should lazily produce values"
                 (Just "hello")
                 [ contT
                 , contT
                 ] $ do
      x <- cont
      y <- cont
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

    it "should not crash for unholyPact" $ do
      analyze channel (pure @(Free Pattern) $ unholyPact @Int)
        `shouldBe` (Nothing, [])

    it "should not crash for unholyPact >>= pure" $ do
      analyze channel (pure @(Free Pattern) (unholyPact @Int) >>= pure)
        `shouldBe` (Nothing, [])

    it "nothing you hold dear is safe" $ do
      let (u, z) = analyze channel $ do
            action $ unholyPact @Int
      evaluate (force (u, z)) `shouldThrow` (== UnholyPact)

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

  describe "channel" $ do
    it "should optimize away its generics" $ do
      $(inspectTest $ hasNoGenerics 'channelPattern)
        `shouldSatisfy` isSuccess


data Pattern a
  = Cont (Bool -> a)
  | Action Int a
  deriving (Functor, Generic1)

instance Eq a => Eq (Pattern a) where
  Cont f == Cont g
      = f True  == g True
     && f False == g False
  Action i a == Action j b
      = i == j
     && a == b
  _ == _ = False

instance Show (Pattern a) where
  show (Cont _)      = "Cont"
  show (Action i _ ) = "Action " ++ show i


instance NFData a => NFData (Pattern a) where
  rnf (Cont f) = seq f ()
  rnf (Action i a) =  seq i ()


cont :: MonadFree Pattern m => m Bool
cont = liftF $ Cont id


action :: MonadFree Pattern m => Int -> m ()
action i = liftF $ Action i ()


channelPattern :: Pattern (Free Pattern a) -> Free Pattern a
channelPattern = channel


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
  let (a, ms) = runAccursed $ improve m
   in it z $ do
        a  `shouldBe` v
        ms `shouldBe` cs

