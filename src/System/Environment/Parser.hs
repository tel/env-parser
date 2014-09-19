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

module System.Environment.Parser where

import           Control.Applicative
import           Control.Exception
import qualified Data.Foldable as F
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

data P a where
  Get :: Key a
      -- ^ The key: where to look up the values in the ENV
      -> (Text -> Either String a)
      -- ^ The mechanism for creating values from ENV text
      -> P a
  deriving Functor

newtype Parser a =
  Parser { unParser :: A P a }
  deriving ( Functor, Applicative )

get :: FromEnv a => Key a -> Parser a
get = Parser . alift . flip Get parseEnv

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

runGetIO :: Map Text Text ->
         -- ^ Overriding map
          P a ->
         IO (Lookup a)
runGetIO m p@(Get k@(Key n _ mdef) go) = do
  case Map.lookup n m of
    Just a  -> return (parser p a)
    Nothing -> do
      t <- getEnvT n
      return $ case t of
        Right a -> parser p a
        Left e  -> case mdef of
          Just (Shown _ a) -> right a
          Nothing          -> failLookup k e

runGetPure :: Map Text Text -> P a -> Lookup a
runGetPure m p@(Get k@(Key n _ mdef) go) = do
  case Map.lookup n m of
    Just a  -> parser p a
    Nothing -> case mdef of
      Just (Shown _ a) -> right a
      Nothing          -> failLookup k Missing

-- | Convert the `Lookup` type to something more naturally palatable
runLookup :: Lookup a -> Either [(Key (), Err)] a
runLookup c = case c of
  Cl s -> Left (F.toList s)
  Cr a -> Right a

-- | Execute a 'Parser' lookup up actual values from the environment
-- only if they are missing from a \"default\" environment mapping.
runParser' :: Map Text Text -> Parser a -> IO (Either [(Key (), Err)] a)
runParser' m = fmap runLookup . decompose . alower (C . runGetIO m) . unParser

-- | Execute a 'Parser' lookup up actual values from the environment.
runParser :: Parser a -> IO (Either [(Key (), Err)] a)
runParser = runParser' Map.empty

-- | Test a parser purely using a mock environment 'Map'
testParser :: Map Text Text -> Parser a -> Either [(Key (), Err)] a
testParser m = runLookup . alower (runGetPure m) . unParser

-- | Extract the list of keys that will be accessed by running the
-- 'Parser'.
documentParser :: Parser a -> [Key ()]
documentParser
  = F.toList . getConst
  . alower (\(Get k _) -> Const . Seq.singleton . forgetKey $ k)
  . unParser
