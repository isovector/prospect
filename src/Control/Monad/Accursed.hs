{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Accursed
  ( -- * Core type
    guess
  , Guess (..)

  -- * Analyzing 'Accursed'
  , explore
  , assay
  , prospect

  -- * Observations
  , given
  , verify
  ) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, throw, catch)
import Control.Monad.Free
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Writer.Strict (runWriter, tell)
import GHC.Generics
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)



------------------------------------------------------------------------------
-- | Perform a best-effort analysis of a free monad.
assay
    :: Functor f
    => (forall b. f (Free f b) -> Free f b)
       -- ^ The following function. Consider using 'explore' to get an
       -- automatic implementation for this.
    -> Free f a
    -> (Maybe a, [f ()])
assay c = runWriter . runMaybeT . go
  where
    go (Pure a) = verify a
    go (Free f) = do
      tell . pure $ () <$ f
      given go $ c f
    {-# INLINE go #-}


verify :: Alternative f => a -> f a
verify = given pure
{-# INLINE verify #-}


given :: Alternative f => (a -> f b) -> a -> f b
given f a = unsafePerformIO $ do
  catch
    (let !_ = a
      in pure $ f a)
    (\(_ :: Guess) -> pure empty)
{-# INLINE given #-}


------------------------------------------------------------------------------
-- | Tear down an 'Accursed' by way of 'explore'.
prospect
    :: (Functor f, Generic1 f, GExplore f (Rep1 f))
    => Free f a
    -> (Maybe a, [f ()])
prospect = assay explore


------------------------------------------------------------------------------
-- | The underlying machinery of 'unholyPact'.
data Guess = Guess
  deriving (Show, Eq)
instance Exception Guess


------------------------------------------------------------------------------
-- | An 'unholyPact' is tretchery whose evaluation can be caught in the
-- 'Accursed' monad. It can be used to follow continuations in a free monad
-- until it branches.
guess :: a
guess = throw Guess


------------------------------------------------------------------------------
-- | Helper class to derive 'explore' generically.
class GExplore p f where
  gexplore :: f (Free p b) -> Free p b

instance TypeError
    (  'Text "Missing continuation parameter when attempting to derive 'explore'"
 ':$$: 'Text "Expected a type variable, but got "
 ':<>: 'ShowType a
    )
      => GExplore p (K1 _1 a) where
  gexplore = undefined
  {-# INLINE gexplore #-}

instance {-# OVERLAPPING #-} TypeError
    (  'Text "Missing continuation parameter when attempting to derive 'explore'"
 ':$$: 'Text "Expected a type variable, but the constructor '"
 ':<>: 'Text tyConName
 ':<>: 'Text "' has none"
    )
      => GExplore p (C1 ('MetaCons tyConName _b _c) U1) where
  gexplore = undefined
  {-# INLINE gexplore #-}

instance GExplore p V1 where
  gexplore _ = undefined
  {-# INLINE gexplore #-}

instance Functor p => GExplore p (Rec1 ((->) a)) where
  gexplore (Rec1 z) = z guess
  {-# INLINE gexplore #-}

instance GExplore p Par1 where
  gexplore (Par1 z) = z
  {-# INLINE gexplore #-}

instance GExplore p g => GExplore p (f :*: g) where
  gexplore (_ :*: b) = gexplore b
  {-# INLINE gexplore #-}

instance (GExplore p f, GExplore p g) => GExplore p (f :+: g) where
  gexplore (L1 f) = gexplore f
  gexplore (R1 g) = gexplore g
  {-# INLINE gexplore #-}

instance GExplore p f => GExplore p (M1 _1 _2 f) where
  gexplore (M1 f) = gexplore f
  {-# INLINE gexplore #-}


------------------------------------------------------------------------------
-- | Generically derived continuation follower; intended to be used as the
-- first parameter for 'assay'.
explore
    :: (Generic1 f, GExplore f (Rep1 f))
    => f (Free f a)
    -> Free f a
explore = gexplore . from1
{-# INLINE explore #-}

