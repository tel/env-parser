{-# LANGUAGE DeriveFunctor        #-}
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
module System.Environment.Parser.Internal where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                 as Ae
import qualified Data.Aeson.Types           as Ae
import qualified Data.Attoparsec.Text       as At
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy       as SL
import qualified Data.ByteString.Lazy.Char8 as SL8
import           Data.Int
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Time
import qualified System.Environment         as Env
import           System.Locale

-- ----------------------------------------------------------------------------
-- Getting to the environment

-- | 'HasEnv' is an adapter abstracting the raw IO-based environment
-- lookup. This lets us simulate a run of our parser in a fake environment.
class Applicative r => HasEnv r where
  getEnv :: String -> r String

instance HasEnv IO where
  getEnv = Env.getEnv


-- ----------------------------------------------------------------------------
-- The Env class

class HasEnv r => Env r where
  liftFailure :: r (Either String a) -> r a

env :: (Env r, FromEnv a) => String -> r a
env = liftFailure . fmap parseEnv . getEnv

-- ----------------------------------------------------------------------------
-- Determining dependencies

-- | The Dep type traverses the leaves of the Env parser tree and
-- determines all of the environment variables which have been accessed.
data Dep a = Dep { findDependencies :: [String] }
  deriving ( Functor )

instance Applicative Dep where
  pure a = Dep []
  Dep s1 <*> Dep s2 = Dep (s1 ++ s2)

instance HasEnv Dep where
  getEnv s = Dep [s]

instance Env Dep where
  liftFailure (Dep s) = Dep s

-- ----------------------------------------------------------------------------
-- Environment types

-- | Types instantiatiating 'FromEnv' can be deserialized from the
-- environment directly.
class FromEnv a where
  parseEnv :: String -> Either String a

instance FromEnv String where
  parseEnv = Right

instance FromEnv S.ByteString where
  parseEnv s = Right (S8.pack s)

instance FromEnv SL.ByteString where
  parseEnv s = Right (SL8.pack s)

instance FromEnv T.Text where
  parseEnv s = Right (T.pack s)

instance FromEnv TL.Text where
  parseEnv s = Right (TL.pack s)

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

json :: (Ae.FromJSON a, Env r) => String -> r a
json = liftFailure . fmap tryConvert . env where
  tryConvert :: Ae.FromJSON a => Ae.Value -> Either String a
  tryConvert = Ae.parseEither Ae.parseJSON

-- ----------------------------------------------------------------------------
-- Utilities

e :: String -> Maybe a -> Either String a
e s Nothing  = Left s
e _ (Just a) = Right a
