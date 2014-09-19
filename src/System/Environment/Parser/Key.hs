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

module System.Environment.Parser.Key (

  -- * Constructing 'Key's
  Key (Key), SomeKey, key, setDefault, setDoc

  -- * Working with 'Key's
  , forgetKey, Shown (..), shown, image
  
  ) where

import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

-- | A 'Key' into the environment. May also come with default values
-- and documentation.
data Key a
  = Key {
      -- | The name of the key
      _keyName :: Text
      -- | A default value and its depiction
    , _defaultValue :: Maybe Text
      -- | A documentation string
    , _docString :: Maybe (Shown a)
    }
  deriving ( Eq, Ord, Show, Functor )

-- | 'Key's which have had their actual default value forgotten. Note:
-- the 'Shown' form of the default value may have been retained for
-- documentation purposes.
type SomeKey = Key ()

-- | The easiest way to construct 'Key' values is using @OverloadedStrings@
instance IsString (Key a) where
  fromString = key . fromString

-- | Constructs a \"bare\" 'Key' without documentation or default
-- value.
key :: Text -> Key a
key t = Key t Nothing Nothing

setDefault :: Shown b -> Key a -> Key b
setDefault s (Key n d _) = Key n d (Just s)

setDoc :: Text -> Key a -> Key a
setDoc d (Key n _ a) = Key n (Just d) a

-- | Forgets the default value stored in the 'Key' if one exists.
forgetKey :: Key a -> Key ()
forgetKey (Key n d a) = Key n d $ fmap (fmap (const ())) a

-- | A 'Shown' value is one which has, at some point in time, had a
-- 'Show'able image.
data Shown a = Shown String a deriving ( Eq, Ord, Show, Functor )

shown :: Show a => a -> Shown a
shown a = Shown (show a) a

image :: Shown a -> String
image (Shown i _) = i
