{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
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
import           Data.Monoid
import qualified Data.Map as Map
import qualified Data.Sequence              as Seq
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

getEnvSafe :: String -> IO (Maybe String)
getEnvSafe key = do
  v <- E.try (getEnv key)
  return $ case v :: Either E.SomeException String of
    Left _  -> Nothing
    Right a -> Just a
  
-- ----------------------------------------------------------------------------
-- The Env class

infix 0 <?>

class (Applicative r, HasEnv r) => Env r where
  -- | Express that a parse has failed
  joinFailure :: r (Either String a) -> r a

  -- | Replace a result with a default if necessary
  def :: a -> (a -> String) -> r a -> r a

  -- | Document a particular branch of the parse
  (<?>) :: r a -> String -> r a
  (<?>) = const

env :: (Env r, FromEnv a) => String -> r a
env = fromEnv . getEnv

defShow :: (Show a, Env r) => a -> r a -> r a
defShow a = def a show

envParse :: (Env r, FromEnv a) => (a -> Either String b) -> String -> r b
envParse parse key = joinFailure $ fmap parse $ env key

-- ----------------------------------------------------------------------------
-- E.g.

data Pth = Pth { pth :: S.ByteString, count :: Int }
  deriving ( Eq, Show )

envPth :: Env r => r Pth
envPth = Pth <$> env "PATH" <*> env "SHLVL"

envPth0 :: Env r => r Pth
envPth0 = Pth <$> env "NOT_THE_PATH" <*> env "SHLVL_BROKEN"

envPth1 :: Env r => r Pth
envPth1 = Pth <$> defShow "foo" (env "NOT_THE_PATH")
              <*> ( (+) <$> env "SHLVL"
                        <*> env "SHLVL"
                        <?> "sum of SHLVL" )

-- ----------------------------------------------------------------------------
-- Dependency tree refinement

data TDep = Succeed
          | Need String
          | TBranch (Seq.Seq TDep)
          | CanFail TDep
          | Default String TDep
          | Documented String TDep
  deriving ( Eq, Show )

data TD a = TD { runTD :: TDep }
  deriving Functor

instance Applicative TD where
  pure _ = TD Succeed
  TD (TBranch tdfs) <*> TD (TBranch tdxs) = TD (TBranch $ tdfs <> tdxs)
  TD (TBranch tdfs) <*> TD tdx            = TD (TBranch $ tdfs Seq.|> tdx)
  TD tdf            <*> TD (TBranch tdxs) = TD (TBranch $ tdf Seq.<| tdxs)
  TD tdf            <*> TD tdx            = TD (TBranch $ Seq.fromList [tdf, tdx])

instance HasEnv TD where
  getEnv key = TD (Need key)

instance Env TD where
  joinFailure (TD tdep)   = TD (CanFail tdep)
  def a sho (TD tdep)     = TD (Default (sho a) tdep)
  TD tdep <?> doc         = TD (Documented doc tdep)

-- ----------------------------------------------------------------------------
-- A smart, failing Parser
--
-- This is much like May above but provides more provocative indications of
-- why a parse failed. Goals are to include all of the ENV vars which would
-- need to be declared (or fixed) in order to make the parse succeed.

data Miss e a = Miss e | Got a
  deriving ( Eq, Ord, Show, Functor )

data Err = Want   String
         | Joined String
  deriving ( Eq, Ord, Show )

instance Monoid e => Applicative (Miss e) where
  pure = Got
  Miss e1 <*> Miss e2 = Miss (e1 <> e2)
  Miss e1 <*> _       = Miss e1
  _       <*> Miss e2 = Miss e2
  Got f   <*> Got x   = Got (f x)

instance HasEnv (Miss [Err]) where
  getEnv key = Miss [Want key]

instance Env (Miss [Err]) where
  joinFailure (Miss er) = Miss er
  joinFailure (Got (Left er))  = Miss [Joined er]
  joinFailure (Got (Right a)) = Got a

  def a _ (Miss _) = Got a
  def _ _ (Got a)  = Got a

  Miss er <?> _doc = Miss er
  Got  a  <?> _doc = Got  a

-- -----------------------------------------------------------------------------
-- Generalized IO wrapper which injects REAL Env parsing into the mix

data Iop f a = Iop { parse :: IO (f a) }
  deriving ( Functor )

instance Applicative f => Applicative (Iop f) where
  pure = Iop . pure . pure
  Iop iof <*> Iop iox = Iop $ liftA2 (<*>) iof iox

instance (Applicative f, HasEnv f) => HasEnv (Iop f) where
  getEnv key = Iop $ maybe (getEnv key) pure <$> getEnvSafe key

instance Env f => Env (Iop f) where
  joinFailure (Iop io) = Iop $ joinFailure <$> io
  def a sho   (Iop io) = Iop $ def a sho   <$> io

-- -----------------------------------------------------------------------------
-- Pureified wrapper which injects FAKE Env parsing into the mix

data Mop f a = Mop { parseFake :: Map.Map String String -> f a }
  deriving ( Functor )

instance Applicative f => Applicative (Mop f) where
  pure = Mop . pure . pure
  Mop mpf <*> Mop mpx = Mop $ liftA2 (<*>) mpf mpx

instance (Applicative f, HasEnv f) => HasEnv (Mop f) where
  getEnv key = Mop $ maybe (getEnv key) pure . Map.lookup key

instance Env f => Env (Mop f) where
  joinFailure (Mop mp) = Mop $ joinFailure <$> mp
  def a sho   (Mop mp) = Mop $ def a sho   <$> mp

-- ----------------------------------------------------------------------------
-- Environment types

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

json :: (Ae.FromJSON a, Env r) => String -> r a
json = envParse (Ae.parseEither Ae.parseJSON)

-- ----------------------------------------------------------------------------
-- Utilities

e :: String -> Maybe a -> Either String a
e s Nothing  = Left s
e _ (Just a) = Right a
