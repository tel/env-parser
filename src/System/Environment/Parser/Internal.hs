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
-- *References*
-- 
-- <http://arxiv.org/abs/1403.0749 /Free Applicative Functors./ Paolo Capriotti, Ambrus Kaposi>
-- 
module System.Environment.Parser.Internal where

import Control.Applicative
import Data.Monoid

--------------------------------------------------------------------------------
-- Internal-only Bimapping

class Bifunctor f where
  bimap :: (l -> l') -> (r -> r') -> (f l r -> f l' r')
  lmap  :: (l -> l')              -> (f l r -> f l' r )
  rmap  ::              (r -> r') -> (f l r -> f l  r')

  bimap f g = lmap f . rmap g
  lmap f = bimap f id
  rmap f = bimap id f

instance Bifunctor Either where
  bimap f g (Left a)  = Left (f a)
  bimap f g (Right a) = Right (g a)

class Bifunctor f => Bipointed f where
  left  :: a -> f a x
  right :: a -> f x a

instance Bipointed Either where
  left  = Left
  right = Right

liftEither :: Bipointed f => Either l r -> f l r
liftEither = either left right

--------------------------------------------------------------------------------
-- Free Applicative Functors

data A f a where
  Pure :: a -> A f a
  (:$) :: f (x -> a) -> A f x -> A f a

-- | Compress the static 'A' structure into the underlying
-- 'Applicative', in effect \"running\" it.
runA :: Applicative f => A f a -> f a
runA = alower id

instance Functor f => Functor (A f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (f :$ x) = fmap (g .) f :$ x

instance Functor f => Applicative (A f) where
  pure = Pure
  Pure f   <*> y = fmap f y
  (f :$ x) <*> y = fmap uncurry f :$ ((,) <$> x <*> y)

-- | Should be called 'liftA' in parallel with 'lift'.
alift :: Functor f => f a -> A f a
alift f = fmap const f :$ Pure ()

-- | Apply an applicative morphism underneath the free
-- transformer. Could be implemented as
--
--     amorph f = lower (alift . f)
--
-- but it would require a 'Functor' constraint on 'g'.
amorph :: Functor g => (forall x . f x -> g x) -> A f a -> A g a
amorph _ (Pure x) = Pure x
amorph k (h :$ x) = k h :$ amorph k x

-- | Interpret each step of the 'A' 'Applicative' into another
-- applicative functor.
alower :: Applicative g => (forall x . f x -> g x) -> A f a -> g a
alower _ (Pure x) = pure x
alower k (g :$ x) = k g <*> alower k x

--------------------------------------------------------------------------------
-- Purely Applicative Either

data Collect l r where
  Cl :: l -> Collect l r
  Cr :: r -> Collect l r
  deriving ( Eq, Ord, Show, Functor )

instance Bifunctor Collect where
  bimap f g (Cl a) = Cl (f a)
  bimap f g (Cr a) = Cr (g a)

instance Monoid l => Applicative (Collect l) where
  pure = Cr
  Cr f <*> Cr x = Cr (f x)
  Cr _ <*> Cl v = Cl v
  Cl u <*> Cr _ = Cl u
  Cl u <*> Cl v = Cl (u <> v)
 
instance Bipointed Collect where
  left  = Cl
  right = Cr

--------------------------------------------------------------------------------
-- Compositions of Applicatives

newtype C f g x = C (f (g x)) deriving ( Eq, Show, Ord, Functor )

decompose :: C f g x -> f (g x)
decompose (C d) = d

instance (Applicative f, Applicative g) => Applicative (C f g) where
  pure = C . pure . pure
  C fgf <*> C fgx = C (liftA2 (<*>) fgf fgx)
