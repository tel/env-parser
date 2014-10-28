{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  System.Environment.Parser.Key
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A 'Key' is a name under which values used in environment
-- configuration can be found. At it's simplest, a 'Key' is just a
-- command to look up a particular value under a particular name in
-- the ENV. It may also contain documentation and or default values as
-- needed, however.
-- 
-- This module is meant to be imported qualified.

module System.Environment.Parser.Key (

  -- * Constructing keys
    Key (Key), make, name, def, doc, def'
  , setName, setDef, setDoc, setDef'

  -- * Generic keys
  , SomeKey, forget
                             
  -- * Working with 'Key's
  , Shown (..), shown, image, value
  
  ) where

import           Control.Applicative
import           Data.String
import           Data.Text (Text)
import           System.Environment.Parser.MicroLens

-- | A 'Key' into the environment. May also come with default values
-- and documentation. The best way to construct a 'Key' is to use
-- @OverloadedStrings@ and then, optionally, set the default values
-- and docstrings using the functions or lenses from
-- "System.Environment.Parser.Key"
data Key a
  = Key !Text !(Maybe (Shown a)) !(Maybe Text)
  deriving ( Eq, Ord, Show, Functor )

-- | 'Key's which have had their actual default value forgotten. Note:
-- the 'Shown' form of the default value may have been retained for
-- documentation purposes.
type SomeKey = Key ()

-- | Forgets the default value stored in the 'Key' if one exists.
forget :: Key a -> Key ()
forget = over def' (fmap (fmap (const ())))
{-# INLINE forget #-}

-- | The easiest way to construct 'Key' values is using @OverloadedStrings@
instance IsString (Key a) where
  fromString = make . fromString

-- | Constructs a \"bare\" 'Key' without documentation or default
-- value.
make :: Text -> Key a
make t = Key t Nothing Nothing

--------------------------------------------------------------------------------
-- Key lenses
--
-- Not exported into the default module, but available if someone
-- imports this module directly.

-- | Lens over the 'name' the 'Key' points to
name :: Functor f => (Text -> f Text) -> (Key a -> f (Key a))
name inj (Key n v d) = (\n' -> Key n' v d) <$> inj n
{-# INLINE name #-}

setName :: Text -> Key a -> Key a
setName = set name
{-# INLINE setName #-}

-- | Lens over the default value the 'Key' takes. Allows more fine
-- modification of the 'Shown' value.
def' :: Functor f => (Maybe (Shown a) -> f (Maybe (Shown b))) -> (Key a -> f (Key b))
def' inj (Key n v d) = (\v' -> Key n v' d) <$> inj v
{-# INLINE def' #-}

-- | Lens over the default value the 'Key' takes. Setting this value
-- via this lens updates the 'Shown' value.
def :: (Functor f, Show b) => (Maybe a -> f (Maybe b)) -> (Key a -> f (Key b))
def inj (Key n v d) =
  (\v' -> Key n (shown <$> v') d)
  <$>
  inj (fmap (\(Shown _ a) -> a) v)
{-# INLINE def #-}

setDef' :: Show b => Maybe (Shown b) -> Key a -> Key b
setDef' = set def'
{-# INLINE setDef' #-}

setDef :: Show b => Maybe b -> Key a -> Key b
setDef = set def
{-# INLINE setDef #-}

-- | Lens over the docstring for a given 'Key'.
doc :: Functor f => (Maybe Text -> f (Maybe Text)) -> Key a -> f (Key a)
doc inj (Key n v d) = Key n v <$> inj d
{-# INLINE doc #-}

setDoc :: Maybe Text -> Key a -> Key a
setDoc = set doc
{-# INLINE setDoc #-}

--------------------------------------------------------------------------------
-- Shown values

-- | A 'Shown' value is one which has, at some point in time, had a
-- 'Show'able image. This is used to store the original image of a
-- default value even if that value itself is forgotten as might be
-- necessary to make a collection of 'Key's.
data Shown a = Shown String a deriving ( Eq, Ord, Functor )

-- | Note that this instance does not demand a @Show a@ constraint.
instance Show (Shown a) where
  show (Shown s _) = s

-- | Produces a 'Shown' value from some 'Show'able value. This fixes a
-- \"snapshot\" of the value so that even if the value itself is later
-- gone it can still be shown.
shown :: Show a => a -> Shown a
shown a = Shown (show a) a

image :: Functor f => (String -> f String) -> Shown a -> f (Shown a)
image inj (Shown i a) = flip Shown a <$> inj i
{-# INLINE image #-}

-- | Modifies a value inside of 'Shown' without changing the image
value :: Functor f => (a -> f b) -> Shown a -> f (Shown b)
value inj (Shown i a) = Shown i <$> inj a
{-# INLINE value #-}
