{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Environment.Parser
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--

module System.Environment.Parser (

  -- * Constructing parsers
    Parser, get, get'
  , Key

  -- * Analyzing and running parsers
  , runParser, runParser'
  , testParser, documentParser
  , Lookup, Errors, errMap, errors
  , Err (..)
  
  ) where

import           Control.Applicative
import           Control.Applicative.Lift
import           Data.Functor.Compose
import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text.Encoding as Te
import qualified Data.Text.Encoding.Error as Te
import           System.Environment.Parser.FromEnv
import           System.Environment.Parser.Internal
import           System.Environment.Parser.Key (Key, SomeKey, Shown (..))
import qualified System.Environment.Parser.Key as Key
import           System.Posix.Env.ByteString

data P a =
  Get {
    -- | The key: where to look up the values in the ENV
      _key :: Key a
    -- | The mechanism for creating values from ENV text
    , _parser :: Text -> Either String a
    }
  deriving Functor

-- | A 'Parser' encodes a sequence of environment lookups used to
-- construct a configuration value.
newtype Parser a =
  Parser { unParser :: A P a }
  deriving ( Functor, Applicative )

-- | Pull a value from the environment using its 'FromEnv' encoding
get :: FromEnv a => Key a -> Parser a
get = get' parseEnv

-- | Pull a value from the environment using a custom parsing function
-- 
-- This exposes a useful general interface for plugging in any kind of
-- value parsing framework. For instance `get` is written
-- 
-- @
-- get = get' parseEnv
-- @
-- 
-- and we an write parsers for JSON values in the environment using
-- Aeson:
-- 
-- @
-- json :: Aeson.FromJSON a => Key a -> Parser a
-- json get' (Aeson.eitherDecode . fromStrict . encodeUtf8)
-- @
get' :: (Text -> Either String a) -> Key a -> Parser a
get' p = Parser . alift . flip Get p

-- | Three possible, handleable error scenarios arise while reading
-- from the environment.
data Err
  = Missing
    -- ^ A value was missing from the environment.
  | ParseError String
    -- ^ The value was found but could not be parsed according to the
    -- given parser.
  | EncodingError Te.UnicodeException
    -- ^ A value was found but could not be interpreted as UTF-8 text.
  deriving ( Eq, Show )

-- | A reasonably efficient error collection type.
type Lookup a = Errors (Seq (SomeKey, Err)) a

-- | Convert the error type in an 'Errors'.
errMap :: (e -> e') -> Errors e a -> Errors e' a
errMap f x = case x of
  Pure a -> Pure a
  Other (Constant e) -> Other (Constant (f e))

-- | Eliminates an 'Errors' type. In order to conver it to an 'Either'
-- use
--
-- @
-- errors Left Right :: Errors e a -> Either e a
-- @
errors :: (e -> c) -> (a -> c) -> (Errors e a -> c)
errors f g x = case x of
  Pure a             -> g a
  Other (Constant e) -> f e

failure1 :: (Key x, Err) -> Lookup a
failure1 (k, e) = failure (Seq.singleton (Key.forget k, e))

-- | Given a mechanism for looking up text from the environment, and
-- possibly failing, extend that lookup to include default value
-- selection and integrated parsing from a 'P'/'Get' action.
goGet :: Monad m => (Text -> m (Errors Err Text)) -> P a -> m (Lookup a)
goGet env (Get k p) = do
  m0 <- env (view Key.name k)
  return $ case m0 of
    Other (Constant Missing) -> case view Key.def' k of
      Just (Shown _ v) -> pure v
      Nothing -> failure1 (k, Missing)
    Other (Constant e) -> failure1 (k, e)
    Pure t -> case p t of
      Left  e -> failure1 (k, ParseError e)
      Right a -> pure a
    
getMap :: Map Text Text -> (Text -> Errors Err Text)
getMap m k = case Map.lookup k m of
  Nothing -> Other (Constant Missing)
  Just a  -> Pure a

getEnvT :: Text -> IO (Errors Err Text)
getEnvT t = do
  m <- getEnv (Te.encodeUtf8 t)
  return $ case m of
    Nothing -> Other (Constant Missing)
    Just a  -> either (Other . Constant . EncodingError) Pure (Te.decodeUtf8' a)

-- | Execute a 'Parser' lookup up actual values from the environment
-- only if they are missing from a \"default\" environment mapping.
runParser' :: Map Text Text -> Parser a -> IO (Lookup a)
runParser' m = getCompose . alower (Compose . getP m) . unParser where
  getP mp p =
    case runIdentity (goGet (pure . getMap mp) p) of
      Pure a -> return (Pure a)
      _      -> goGet getEnvT p

-- | Execute a 'Parser' lookup up actual values from the environment.
runParser :: Parser a -> IO (Lookup a)
runParser = getCompose . alower (Compose . goGet getEnvT) . unParser

-- | Test a parser purely using a mock environment 'Map'
testParser :: Map Text Text -> Parser a -> Lookup a
testParser m = alower (runIdentity . goGet (pure . getMap m)) . unParser

-- | Extract the list of keys that will be accessed by running the
-- 'Parser'.
documentParser :: Parser a -> Seq SomeKey
documentParser
  = getConstant
  . alower (\(Get k _) -> Constant . Seq.singleton . Key.forget $ k)
  . unParser
