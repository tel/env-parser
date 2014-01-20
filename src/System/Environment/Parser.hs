{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : System.Environment.Parser
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Functions for building generic environment parsers which provide
-- automatic documentation and easy testing.
--
module System.Environment.Parser (

  -- * Basic interface

    run       -- :: Parser a -> IO (Either Errors a)
  , test      -- :: Parser a -> Map.Map String String -> Either Errors a
  , help      -- :: Parser a -> Dep
  , get       -- :: FromEnv a  => String -> Parser a
  , getParse  -- :: FE.FromEnv a => (a -> Either String b) -> String -> Parser b
  , json      -- :: FromJSON a => String -> Parser a

  -- * Interface types

  , Parser
  , Errors (..), Err (..)
  , Dep    (..), references

  ) where

import           Control.Applicative
import qualified Data.Aeson                        as Ae
import qualified Data.Aeson.Types                  as Ae
import           Data.Functor.Compose
import qualified Data.Map                          as Map
import           Data.Monoid
import           Data.Foldable                     (toList, foldMap)
import           Data.Sequence                     ((<|), (|>))
import qualified Data.Sequence                     as Seq
import qualified System.Environment.Parser.Class   as Cls
import qualified System.Environment.Parser.FromEnv as FE
import           System.Environment.Parser.Miss

-- -----------------------------------------------------------------------------
-- Error types

data Err = Wanted String | Joined String
  deriving ( Eq, Ord, Show )

newtype Errors = Errors { getErrors :: Seq.Seq Err }
  deriving ( Eq, Ord, Show, Monoid )

instance Cls.Satisfiable Errors where
  wants  = Errors . Seq.singleton . Wanted
  errors = Errors . Seq.singleton . Joined

-- -----------------------------------------------------------------------------
-- Analysis types

data Dep = Succeeding
         | Needing    String
         | Branching  (Seq.Seq Dep)
         | Joining    Dep
         | Defaulting String Dep
  deriving ( Eq, Show )

references :: Dep -> [String]
references = toList . foldDep where
  foldDep Succeeding       = Seq.empty
  foldDep (Needing key)    = Seq.singleton key
  foldDep (Branching ds)   = foldMap foldDep ds
  foldDep (Joining d)      = foldDep d
  foldDep (Defaulting _ d) = foldDep d

data Df a = Df { runDf :: Dep } deriving Functor

instance Applicative Df where
  pure _ = Df Succeeding
  Df (Branching dfs) <*> Df (Branching dxs) = Df (Branching $ dfs <> dxs)
  Df (Branching dfs) <*> Df dx              = Df (Branching $ dfs |> dx)
  Df df              <*> Df (Branching dxs) = Df (Branching $ df <| dxs)
  Df df              <*> Df dx              = Df (Branching $ Seq.fromList [df, dx])

instance Cls.HasEnv Df where
  getEnv key = Df (Needing key)

instance Cls.Env Df where
  joinFailure (Df dep)   = Df (Joining dep)
  def a sho (Df dep)     = Df (Defaulting (sho a) dep)

-- -----------------------------------------------------------------------------
-- Parser types

data Parser a = Parser
  { run'  :: Compose IO (Miss Errors) a
  , test' :: Compose ((->) (Map.Map String String)) (Miss Errors) a
  , help' :: Df a
  }
  deriving ( Functor )

instance Applicative Parser where
  pure a = Parser (pure a) (pure a) (pure a)
  Parser f1 f2 f3 <*> Parser x1 x2 x3 =
    Parser (f1 <*> x1) (f2 <*> x2) (f3 <*> x3)

instance Cls.HasEnv Parser where
  getEnv key = Parser (Cls.getEnv key) (Cls.getEnv key) (Cls.getEnv key)

instance Cls.Env Parser where
  joinFailure (Parser i1 i2 i3) =
    Parser (Cls.joinFailure i1) (Cls.joinFailure i2) (Cls.joinFailure i3)
  def a sho   (Parser i1 i2 i3) =
    Parser (Cls.def a sho i1) (Cls.def a sho i2) (Cls.def a sho i3)

run :: Parser a -> IO (Either Errors a)
run = fmap toEither . getCompose . run'

test :: Parser a -> Map.Map String String -> Either Errors a
test = fmap toEither . getCompose . test'

help :: Parser a -> Dep
help = runDf . help'

get :: FE.FromEnv a => String -> Parser a
get = FE.fromEnv . Cls.getEnv

json :: Ae.FromJSON a => String -> Parser a
json = getParse (Ae.parseEither Ae.parseJSON)

getParse :: FE.FromEnv a => (a -> Either String b) -> String -> Parser b
getParse parse key = Cls.joinFailure $ fmap parse $ get key
