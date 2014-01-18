{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : System.Environment.Parser.Encoded
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Often it's useful to pass binary data through the environment as an
-- encoded string. This module provides handy types for specifying these
-- kinds of data.
--
-- @
-- data SecretKeys = Sk { key1 :: Base64, key2 :: Base64 }
-- @
--
module System.Environment.Parser.Encoded (

  Base64 (..), Base64Url (..), Base16 (..)
                                         
 ) where

import           Control.Monad
import qualified Data.ByteString                    as S
import qualified Data.ByteString.Base16             as S16
import qualified Data.ByteString.Base64             as S64
import qualified Data.ByteString.Base64.URL         as S64U
import           System.Environment.Parser.Internal

-- | Isomorphic to a 'S.ByteString', a type which prefers to be base-64
-- encoded.
newtype Base64 = Base64 { unBase64 :: S.ByteString }
  deriving ( Eq, Ord, Show )

-- | Isomorphic to a 'S.ByteString', a type which prefers to be base-64-url
-- encoded (see <http://www.apps.ietf.org/rfc/rfc4648.html>).
newtype Base64Url = Base64Url { unBase64Url :: S.ByteString }
  deriving ( Eq, Ord, Show )

-- | Isomorphic to a 'S.ByteString', a type which prefers to be hexadecimal
-- encoded.
newtype Base16 = Base16 { unBase16 :: S.ByteString }
  deriving ( Eq, Ord, Show )

instance FromEnv Base64 where
  parseEnv = fmap Base64 . S64.decode <=< parseEnv

instance FromEnv Base64Url where
  parseEnv = fmap Base64Url . S64U.decode <=< parseEnv

instance FromEnv Base16 where
  parseEnv = fmap Base16 . s16decode <=< parseEnv where
    s16decode :: S.ByteString -> Either String S.ByteString
    s16decode bs = case S16.decode bs of
      (out, rest) | S.null rest -> Right out
                  | otherwise   -> Left "failed to decode hexadecimal string"
