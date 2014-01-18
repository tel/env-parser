
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

-- ----------------------------------------------------------------------------
-- Getting to the environment

-- | 'HasEnv' is an adapter abstracting the raw IO-based environment
-- lookup. This lets us simulate a run of our parser in a fake environment.
class HasEnv m where
  getEnv :: String -> m String

instance HasEnv IO where
  getEnv = Env.getEnv


-- ----------------------------------------------------------------------------
-- A sneaky applicative

class HasEnv m => Env m where
  may :: m (Either String a) -> m a


-- ----------------------------------------------------------------------------
-- Environment types

-- | Types instantiatiating 'FromEnv' can be deserialized from the
-- environment directly.
class FromEnv a where
  parseEnv :: String -> Either String a

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


