{-# LANGUAGE RecordWildCards #-}
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
  , run, run'
  , test, document
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
import           Data.Monoid
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text.Encoding as Te
import qualified Data.Text.Encoding.Error as Te
import           System.Environment.Parser.FromEnv
import           System.Environment.Parser.Key (Key, SomeKey, Shown (..))
import qualified System.Environment.Parser.Key as Key
import           System.Environment.Parser.MicroLens
import           System.Posix.Env.ByteString

-- | An 'Either' analogue, but which accumulates monoidal errors for
-- its Applicative instance.
type Lookup a = Errors (Seq (SomeKey, Err)) a

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

data Parser a =
  Parser
  { runIO   :: Map Text Text -> IO (Lookup a)
  , runPure :: Map Text Text -> Lookup a
  , runDocs :: Seq SomeKey
  } deriving Functor

instance Applicative Parser where
  pure a = Parser {..} where
    runIO   _ = pure (pure a)
    runPure _ = pure a
    runDocs   = mempty
  pf <*> px =
    Parser
    { runIO   = \mp -> liftA2 (<*>) (runIO pf mp) (runIO px mp)
    , runPure = \mp -> runPure pf mp <*> runPure px mp
    , runDocs = runDocs pf <> runDocs px
    }

--------------------------------------------------------------------------------

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
get' par k = Parser {..} where
  -- runIO :: Map Text Text -> IO (Lookup a)
  runIO mp = do
    mv <- getEnv (Te.encodeUtf8 $ view Key.name k)
    return $ case mv of
      Nothing -> (runPure mp)
      Just bs -> case Te.decodeUtf8' bs of
        Left err -> failure1 k (EncodingError err)
        Right t  -> case par t of
          Left err -> failure1 k (ParseError err)
          Right v  -> pure v
  -- runPure :: Map Text Text -> Lookup a
  runPure mp = case Map.lookup (view Key.name k) mp of
    Nothing -> case view Key.def' k of
      Nothing          -> failure1 k Missing
      Just (Shown _ v) -> pure v
    Just t -> case par t of
      Left err -> failure1 k (ParseError err)
      Right v  -> pure v
  runDocs = Seq.singleton (Key.forget k)

failure1 :: Key a1 -> t -> Errors (Seq (Key (), t)) a
failure1 k e = failure (Seq.singleton (Key.forget k, e))

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | Execute a 'Parser' lookup up actual values from the environment
-- only if they are missing from a \"default\" environment mapping.
run' :: Map Text Text -> Parser a -> IO (Lookup a)
run' = flip runIO

-- | Execute a 'Parser' lookup up actual values from the environment.
run :: Parser a -> IO (Lookup a)
run = flip runIO mempty

-- | Test a parser purely using a mock environment 'Map'
test :: Map Text Text -> Parser a -> Lookup a
test = flip runPure

-- | Extract the list of keys that will be accessed by running the
-- 'Parser'.
document :: Parser a -> Seq SomeKey
document = runDocs
