{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : System.Environment.Parser.Internal
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Types which can be deserialized from an environment variable.

module System.Environment.Parser.FromEnv (

  FromEnv (..)

  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                      as Ae
import qualified Data.Attoparsec.Text            as At
import qualified Data.ByteString                 as S
import qualified Data.ByteString.Char8           as S8
import qualified Data.ByteString.Lazy            as SL
import qualified Data.ByteString.Lazy.Char8      as SL8
import           Data.Int
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import           Data.Time
import           System.Environment.Parser.Class
import           System.Locale

-- | Types instantiatiating 'FromEnv' can be deserialized from the
-- environment directly.
class FromEnv a where
  parseEnv :: String -> Either String a

  -- | For this most part this should be left as default. It's useful for
  -- introducing non-failing parsers, though.
  fromEnv :: Env r => r String -> r a
  fromEnv = joinFailure . fmap parseEnv


instance FromEnv String where
  parseEnv = Right
  fromEnv = id

instance FromEnv S.ByteString where
  parseEnv s = Right (S8.pack s)
  fromEnv = fmap S8.pack

instance FromEnv SL.ByteString where
  parseEnv s = Right (SL8.pack s)
  fromEnv = fmap SL8.pack

instance FromEnv T.Text where
  parseEnv s = Right (T.pack s)
  fromEnv = fmap T.pack

instance FromEnv TL.Text where
  parseEnv s = Right (TL.pack s)
  fromEnv = fmap TL.pack

integralEnv :: Integral a => String -> Either String a
integralEnv s = do
  txt <- parseEnv s
  At.parseOnly (At.signed At.decimal) txt where

instance FromEnv Int     where parseEnv = integralEnv
instance FromEnv Integer where parseEnv = integralEnv
instance FromEnv Int8    where parseEnv = integralEnv
instance FromEnv Int64   where parseEnv = integralEnv
instance FromEnv Int32   where parseEnv = integralEnv
instance FromEnv Int16   where parseEnv = integralEnv

instance FromEnv Double where
  parseEnv s = do
    txt <- parseEnv s
    At.parseOnly (At.signed At.double) txt

instance FromEnv At.Number where
  parseEnv s = do
    txt <- parseEnv s
    At.parseOnly (At.signed At.number) txt

-- ----------------------------------------------------------------------------
-- Time parsers
--
-- These may not always be the most apropriate formats for parsing time,
-- but customer parser can always be appended as needed. Instead, these
-- provide convention.

-- | Interprets a string as a decimal number of seconds
instance FromEnv DiffTime where
  parseEnv s =
    realToFrac <$> (parseEnv s :: Either String At.Number)

-- | Interprets a string as a decimal number of seconds
instance FromEnv NominalDiffTime where
  parseEnv s =
    realToFrac <$> (parseEnv s :: Either String At.Number)

-- | Assumes first that the date is formatted as the W3C Profile of ISO
-- 8601 but also implements a few other formats.
--
-- > %Y-%m-%dT%H:%M:%S%Q%z
-- >
-- > 1997-07-16T19:20:30.45+01:00
-- > 1997-07-16T19:20:30.45Z
-- > 1997-07-16T19:20:30Z
--
-- > %a %b %_d %H:%M:%S %z %Y
-- >
-- > Sat Jan 18 22:20:02 +0000 2014
-- > Sat Jan 18 22:20:02 2014
-- > Jan 18 22:20:02 2014
--
instance FromEnv UTCTime where
  parseEnv s =
    e "bad UTC time"
    $ msum $ map (\format -> parseTime defaultTimeLocale format s) formats

    where
      formats =
        [ "%Y-%m-%dT%H:%M:%S%Q%z"
        , "%Y-%m-%dT%H:%M:%S%QZ"
        , "%a %b %_d %H:%M:%S %z %Y"
        , "%a %b %_d %H:%M:%S %Y"
        , "%b %_d %H:%M:%S %Y"
        ]

-- | Parses the Gregorian calendar format @\"%Y-%m-%d\"@.
instance FromEnv Day where
  parseEnv s =
    e "bad date" $ parseTime defaultTimeLocale "%Y-%m-%d" s


-- ----------------------------------------------------------------------------
-- JSON Parsers
--
-- JSON is such a convenient format that it might be conceivably jammed
-- into an environment variable. Since Aeson will soon be in the Haskell
-- platform we'll go ahead and include some obvious default instances for
-- Aeson Value types along with a nice general parser.

instance FromEnv Ae.Value where
  parseEnv s = do
    bs <- parseEnv s
    Ae.eitherDecodeStrict bs

-- ----------------------------------------------------------------------------
-- Utilities

e :: String -> Maybe a -> Either String a
e s Nothing  = Left s
e _ (Just a) = Right a
