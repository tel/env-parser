{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : System.Environment.Parser.MicroLens
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 

module System.Environment.Parser.MicroLens (

  view, over, set, O, O'
  
  ) where

import Data.Functor.Identity
import Control.Applicative

type O  p s t a b = ((a -> p b) -> (s -> p t))
type O' p s a     = O p s s a a

view :: O (Const a) s t a b -> (s -> a)
view l = getConst . l Const
{-# INLINE view #-}

over :: O Identity s t a b -> (a -> b) -> (s -> t)
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

set :: O Identity s t a b -> b -> (s -> t)
set l = over l . const
{-# INLINE set #-}
