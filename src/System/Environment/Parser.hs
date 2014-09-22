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

  -- * Analyzing and running parsers
  , runParser, runParser'
  , testParser, documentParser
  , ParserReport
  , Err (..)
  
  ) where

import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Exception
import qualified Data.ByteString.Lazy as Sl
import qualified Data.Foldable as F
import           Data.Functor.Compose
import           Data.Functor.Constant
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Text.Encoding.Error as Te
import           System.Environment.Parser.FromEnv
import           System.Environment.Parser.Internal
import           System.Environment.Parser.Key
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
--     get = get' parseEnv
-- 
-- and we an write parsers for JSON values in the environment using
-- Aeson:
-- 
--     json :: Aeson.FromJSON a => Key a -> Parser a
--     json get' (Aeson.eitherDecode . fromStrict . encodeUtf8)
get' :: (Text -> Either String a) -> Key a -> Parser a
get' p = Parser . alift . flip Get p

data Err
  = Missing
  | ParseError String
  | EncodingError Te.UnicodeException
  deriving ( Eq, Show )

type Lookup a = Errors (Seq (SomeKey, Err)) a

failLookup :: Key a -> Err -> Lookup x
failLookup k err = failure (Seq.singleton (forgetKey k, err))

parser :: P a -> Text -> Lookup a
parser (Get k go) text = case go text of
  Right a  -> pure a
  Left err -> failLookup k (ParseError err)

eitherl :: (e -> e') -> Either e a -> Either e' a
eitherl f = either (Left . f) Right

getEnvT :: Text -> IO (Either Err Text)
getEnvT t = do
  m <- getEnv (Te.encodeUtf8 t)
  return $ case m of
    Nothing -> Left Missing
    Just a  -> eitherl EncodingError (Te.decodeUtf8' a)

runGetIO :: Map Text Text -- ^ Overriding map
         -> P a -> IO (Lookup a)
runGetIO m p@(Get k@(Key n _ mdef) go) = do
  case Map.lookup n m of
    Just a  -> return (parser p a)
    Nothing -> do
      t <- getEnvT n
      return $ case t of
        Right a -> parser p a
        Left e  -> case mdef of
          Just (Shown _ a) -> pure a
          Nothing          -> failLookup k e

runGetPure :: Map Text Text -> P a -> Lookup a
runGetPure m p@(Get k@(Key n _ mdef) go) = do
  case Map.lookup n m of
    Just a  -> parser p a
    Nothing -> case mdef of
      Just (Shown _ a) -> pure a
      Nothing          -> failLookup k Missing

type ParserReport = Either [(SomeKey, Err)]

-- | Convert the `Lookup` type to something more naturally palatable
runLookup :: Lookup a -> ParserReport a
runLookup c = case c of
  Other (Constant s) -> Left (F.toList s)
  Pure a -> Right a

-- | Execute a 'Parser' lookup up actual values from the environment
-- only if they are missing from a \"default\" environment mapping.
runParser' :: Map Text Text -> Parser a -> IO (ParserReport a)
runParser' m = fmap runLookup . getCompose . alower (Compose . runGetIO m) . unParser

-- | Execute a 'Parser' lookup up actual values from the environment.
runParser :: Parser a -> IO (ParserReport a)
runParser = runParser' Map.empty

-- | Test a parser purely using a mock environment 'Map'
testParser :: Map Text Text -> Parser a -> ParserReport a
testParser m = runLookup . alower (runGetPure m) . unParser

-- | Extract the list of keys that will be accessed by running the
-- 'Parser'.
documentParser :: Parser a -> [SomeKey]
documentParser
  = F.toList . getConst
  . alower (\(Get k _) -> Const . Seq.singleton . forgetKey $ k)
  . unParser
