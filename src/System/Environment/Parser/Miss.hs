{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      : System.Environment.Parser.Miss
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- A purely Applicative Either.

module System.Environment.Parser.Miss where

import           Control.Applicative
import           Data.Monoid

data Miss e a
  = Got  a
  | Miss e
  deriving ( Eq, Ord, Show, Read, Functor )

instance Monoid e => Applicative (Miss e) where
  pure = Got
  Miss e1 <*> Miss e2 = Miss (e1 <> e2)
  Miss e1 <*> _       = Miss e1
  _       <*> Miss e2 = Miss e2
  Got f   <*> Got x   = Got  (f x)

toEither :: Miss e a -> Either e a
toEither (Miss e) = Left e
toEither (Got  a) = Right a

missMap :: (e -> f) -> Miss e a -> Miss f a
missMap f (Miss e) = Miss (f e)
missMap _ (Got  a) = Got a

gotMap :: (a -> b) -> Miss e a -> Miss e b
gotMap _ (Miss e) = Miss e
gotMap f (Got  a) = Got (f a)
