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
import Control.Monad.Accursed
import Control.Monad.Codensity
import Control.Monad.Free (MonadFree, liftF)
import GHC.Generics
import Test.Hspec
import Test.Inspection



main :: IO ()
main = hspec $ do
  let contT = Cont $ const ()
  describe "Accursed" $ do
    testAccursed "should lazily produce values"
                 (Just "hello")
                 [ contT
                 , contT
                 ] $ do
      x <- cont
      y <- cont
      pure "hello"

    testAccursed "should not be cursed for producer patterns"
                 (Just ())
                 [ Action 1 ()
                 , Action 2 ()
                 ] $ do
      action 1
      action 2

    testAccursed "should not crash for a single continuation"
                 Nothing
                 [ contT
                 ] $ do
      cont

    testAccursed "should stop on an explicit bang pattern"
                 Nothing
                 [ contT
                 ] $ do
      !x <- cont
      pure "error"

    it "should not crash for curse" $ do
      analyze channel (curse @Pattern @Int)
        `shouldBe` (Nothing, [])

    it "should not crash for curse >>= pure" $ do
      analyze channel (curse @Pattern @Int >>= pure)
        `shouldBe` (Nothing, [])

    testAccursed "should stop before branching"
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


  describe "Channel" $ do
    it "should optimize away its generics" $ do
      $(inspectTest $ hasNoGenerics 'channelPattern) `shouldSatisfy` isSuccess


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

instance Show (Pattern a) where
  show (Cont _)      = "Cont"
  show (Action _ _ ) = "Action"


cont :: MonadFree Pattern m => m Bool
cont = liftF $ Cont id


action :: MonadFree Pattern m => Int -> m ()
action i = liftF $ Action i ()


channelPattern :: Pattern (Accursed Pattern a) -> Accursed Pattern a
channelPattern = channel


isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False


testAccursed
    :: (Eq a, Show a)
    => String
    -> Maybe a
    -> [Pattern ()]
    -> (forall m. MonadFree Pattern m => m a)
    -> SpecWith (Arg Expectation)
testAccursed z v cs m =
  let (a, ms) = runAccursed . corrupt $ improve m
   in it z $ do
        a  `shouldBe` v
        ms `shouldBe` cs

