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
import qualified Control.Exception          as E
import           Control.Monad
import qualified Data.Aeson                 as Ae
import qualified Data.Aeson.Types           as Ae
import qualified Data.Attoparsec.Text       as At
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy       as SL
import qualified Data.ByteString.Lazy.Char8 as SL8
import           Data.Int
import           Data.List                  (intersect)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Time
import qualified System.Environment         as Env
import           System.Locale

-- ----------------------------------------------------------------------------
-- Getting to the environment

-- | 'HasEnv' is an adapter abstracting the raw IO-based environment
-- lookup. This lets us simulate a run of our parser in a fake environment.
class HasEnv r where
  getEnv :: String -> r String

instance HasEnv IO where
  getEnv = Env.getEnv


-- ----------------------------------------------------------------------------
-- The Env class

class (Alternative r, HasEnv r) => Env r where
  -- | Express that a parse has failed
  joinFailure :: r (Either String a) -> r a

  -- | Document a particular branch of the parse
  (<?>) :: r a -> String -> r a
  (<?>) = const

env :: (Env r, FromEnv a) => String -> r a
env = joinFailure . fmap parseEnv . getEnv

envParse :: (Env r, FromEnv a) => (a -> Either String b) -> String -> r b
envParse parse key = joinFailure $ fmap parse $ env key

-- ----------------------------------------------------------------------------
-- E.g.

data Pth = Pth { pth :: S.ByteString }
  deriving ( Eq, Show )

envPth :: Env r => r Pth
envPth = Pth <$> env "PATH"

envPth0 :: Env r => r Pth
envPth0 = Pth <$> env "NOT_THE_PATH"

-- ----------------------------------------------------------------------------
-- Determining dependencies

-- | The Dep type traverses the leaves of the Env parser tree and
-- determines all of the environment variables which have been accessed.
data Dep a = Dep { findDependencies :: [String] }
  deriving ( Functor )

instance Applicative Dep where
  pure a = Dep []
  Dep s1 <*> Dep s2 = Dep (s1 ++ s2)

instance Alternative Dep where
  empty = Dep ["{unsatisfiable}"]
  Dep as <|> Dep bs = Dep (as `intersect` bs)

instance HasEnv Dep where
  getEnv s = Dep [s]

instance Env Dep where
  joinFailure (Dep s) = Dep s
  (<?>) = const

-- ----------------------------------------------------------------------------
-- A dumb, failing Parser
--
-- Instead of taking advantage of the Applicative structure to pass around
-- reasons for failure, this version just straight-up fails.

data May a = May { runMay :: IO (Maybe a) }
  deriving Functor

instance Applicative May where
  pure = May . pure . pure
  May iof <*> May iox = May $ liftA2 (<*>) iof iox

instance Alternative May where
  empty = May (pure Nothing)
  May io1 <|> May io2 = May $ liftA2 (<|>) io1 io2

instance HasEnv May where
  getEnv key = May $ do
    v <- E.try (getEnv key)
    case v of
      Left e  -> let _ = (e :: E.SomeException) in return Nothing
      Right a -> return (Just a)

instance Env May where
  joinFailure (May io) = May $ do
    x <- io
    case x of
      Nothing -> return Nothing
      Just y  -> case y of
        Left _  -> return Nothing
        Right a -> return (Just a)

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
json = envParse (Ae.parseEither Ae.parseJSON)

-- ----------------------------------------------------------------------------
-- Utilities

e :: String -> Maybe a -> Either String a
e s Nothing  = Left s
e _ (Just a) = Right a
