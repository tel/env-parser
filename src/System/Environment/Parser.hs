{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : System.Environment.Parser
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--

module System.Environment.Parser where

import           Control.Applicative
import           Control.Exception
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

data P a where
  Get :: Key a
      -- ^ The key to look up the values in the ENV using
      -> (Text -> Either String a)
      -- ^ The mechanism for creating values from ENV text
      -> P a
  deriving Functor

get :: FromEnv a => Key a -> A P a
get = alift . flip Get parseEnv

data Err
  = Missing
  | ParseError String
  | EncodingError Te.UnicodeException
  deriving ( Eq, Show )

type Lookup a = Collect (Seq (Key (), Err)) a

failLookup :: Key a -> Err -> Lookup x
failLookup k err = left (Seq.singleton (forgetKey k, err))

parser :: P a -> Text -> Lookup a
parser (Get k go) text = case go text of
  Right a  -> right a
  Left err -> failLookup k (ParseError err)

getEnvT :: Text -> IO (Either Err Text)
getEnvT t = do
  m <- getEnv (Te.encodeUtf8 t)
  return $ case m of
    Nothing -> Left Missing
    Just a  -> lmap EncodingError (Te.decodeUtf8' a)
  
runGet :: P a -> IO (Lookup a)
runGet p@(Get k@(Key n _ mdef) go) = do
  t <- getEnvT n
  return $ case t of
    Right a -> parser p a
    Left e  -> case mdef of
      Just (Shown _ a) -> right a
      Nothing          -> failLookup k e
  
ex :: A P (Int, Int)
ex = (,) <$> get "TZ" <*> get "FOO"

run :: A P a -> IO (Lookup a)
run = decompose . alower (C . runGet)

