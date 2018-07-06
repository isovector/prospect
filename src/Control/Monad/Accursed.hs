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
    unholyPact
  , UnholyPact (..)

  -- * Analyzing 'Accursed'
  , channel
  , analyze
  , runAccursed
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
analyze
    :: Functor f
    => (forall b. f (Free f b) -> Free f b)
       -- ^ The following function. Consider using 'channel' to get an
       -- automatic implementation for this.
    -> Free f a
    -> (Maybe a, [f ()])
analyze c = runWriter . runMaybeT . go
  where
    go (Pure a) = unsafePerformIO $
      catch
        (let !_ = a
          in pure $ pure a)
        (\(_ :: UnholyPact) -> pure empty)
    go (Free f) = do
      tell . pure $ () <$ f
      unsafePerformIO $
        catch
          ( let !z = c f
             in pure $ go z)
          (\(_ :: UnholyPact) -> pure empty)
    {-# INLINE go #-}


------------------------------------------------------------------------------
-- | Tear down an 'Accursed' by way of 'channel'.
runAccursed
    :: (Functor f, Generic1 f, GChannel f (Rep1 f))
    => Free f a
    -> (Maybe a, [f ()])
runAccursed = analyze channel


------------------------------------------------------------------------------
-- | The underlying machinery of 'unholyPact'.
data UnholyPact = UnholyPact
  deriving (Show, Eq)
instance Exception UnholyPact


------------------------------------------------------------------------------
-- | An 'unholyPact' is tretchery whose evaluation can be caught in the
-- 'Accursed' monad. It can be used to follow continuations in a free monad
-- until it branches.
unholyPact :: a
unholyPact = throw UnholyPact


------------------------------------------------------------------------------
-- | Helper class to derive 'channel' generically.
class GChannel p f where
  gchannel :: f (Free p b) -> Free p b

instance TypeError
    (  'Text "Missing continuation parameter when attempting to derive 'channel'"
 ':$$: 'Text "Expected a type variable, but got "
 ':<>: 'ShowType a
    )
      => GChannel p (K1 _1 a) where
  gchannel = undefined
  {-# INLINE gchannel #-}

instance {-# OVERLAPPING #-} TypeError
    (  'Text "Missing continuation parameter when attempting to derive 'channel'"
 ':$$: 'Text "Expected a type variable, but the constructor '"
 ':<>: 'Text tyConName
 ':<>: 'Text "' has none"
    )
      => GChannel p (C1 ('MetaCons tyConName _b _c) U1) where
  gchannel = undefined
  {-# INLINE gchannel #-}

instance GChannel p V1 where
  gchannel _ = undefined
  {-# INLINE gchannel #-}

instance Functor p => GChannel p (Rec1 ((->) a)) where
  gchannel (Rec1 z) = z unholyPact
  {-# INLINE gchannel #-}

instance GChannel p Par1 where
  gchannel (Par1 z) = z
  {-# INLINE gchannel #-}

instance GChannel p g => GChannel p (f :*: g) where
  gchannel (_ :*: b) = gchannel b
  {-# INLINE gchannel #-}

instance (GChannel p f, GChannel p g) => GChannel p (f :+: g) where
  gchannel (L1 f) = gchannel f
  gchannel (R1 g) = gchannel g
  {-# INLINE gchannel #-}

instance GChannel p f => GChannel p (M1 _1 _2 f) where
  gchannel (M1 f) = gchannel f
  {-# INLINE gchannel #-}


------------------------------------------------------------------------------
-- | Generically derived continuation follower; intended to be used as the
-- first parameter for 'analyze'.
channel
    :: (Generic1 f, GChannel f (Rep1 f))
    => f (Free f a)
    -> Free f a
channel = gchannel . from1
{-# INLINE channel #-}

