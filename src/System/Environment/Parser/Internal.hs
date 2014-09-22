{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


-- |
-- Module      : System.Environment.Parser.Internal
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- This module contains a number of supporting types and functionality
-- that can be useful for understanding or using @env-parser@ but is
-- not considered part of the public interface and thus is subject to
-- change.
-- 
-- /References/
-- 
--   * <http://arxiv.org/abs/1403.0749 "Free Applicative Functors" Paolo Capriotti, Ambrus Kaposi>
-- 
module System.Environment.Parser.Internal where

import Control.Applicative
import Data.Monoid

--------------------------------------------------------------------------------
-- Free Applicative Functors

data A f a where
  Inj :: a -> A f a
  (:$) :: f (x -> a) -> A f x -> A f a

-- | Compress the static 'A' structure into the underlying
-- 'Applicative', in effect \"running\" it.
runA :: Applicative f => A f a -> f a
runA = alower id

instance Functor f => Functor (A f) where
  fmap g (Inj a)  = Inj (g a)
  fmap g (f :$ x) = fmap (g .) f :$ x

instance Functor f => Applicative (A f) where
  pure = Inj
  Inj f    <*> y = fmap f y
  (f :$ x) <*> y = fmap uncurry f :$ ((,) <$> x <*> y)

-- | Should be called 'liftA' in parallel with 'lift'.
alift :: Functor f => f a -> A f a
alift f = fmap const f :$ Inj ()

-- | Apply an applicative morphism underneath the free
-- transformer. Could be implemented as
--
--     amorph f = lower (alift . f)
--
-- but it would require a 'Functor' constraint on 'g'.
amorph :: Functor g => (forall x . f x -> g x) -> A f a -> A g a
amorph _ (Inj x)  = Inj x
amorph k (h :$ x) = k h :$ amorph k x

-- | Interpret each step of the 'A' 'Applicative' into another
-- applicative functor.
alower :: Applicative g => (forall x . f x -> g x) -> A f a -> g a
alower _ (Inj x)  = pure x
alower k (g :$ x) = k g <*> alower k x
