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

  -- ** Building a ''Parser'

  Parser
  
  , get       -- :: FromEnv a  => String -> Parser a
  , getParse  -- :: FE.FromEnv a => (a -> Either String b) -> String -> Parser b
  , json      -- :: FromJSON a => String -> Parser a

  -- *** Annotating a 'Parser'
  , def       -- :: Show a => a -> Parser a -> Parser a
  , def'      -- ::           a -> Parser a -> Parser a
  , doc       -- :: String -> Parser a -> Parser a

  -- * Interpreting a 'Parser'
    
  , run       -- :: Parser a -> IO (Either Errors a)
  , test      -- :: Parser a -> Map.Map String String -> Either Errors a
  , analyze   -- :: Parser a -> Analysis

  , Errors (..), Err (..)

  -- ** Analysis and documentation
  , Analysis (..), references

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

-- | We consider two broad classes of failure: either an environment
-- variable was expected to exist and didn't (it was 'Wanted') or the
-- value failed to validate during parsing and we've 'Joined' an
-- error message from that failed parse.
data Err = Wanted String | Joined String
  deriving ( Eq, Ord, Show )

newtype Errors = Errors { getErrors :: Seq.Seq Err }
  deriving ( Eq, Ord, Show, Monoid )

instance Cls.Satisfiable Errors where
  wants  = Errors . Seq.singleton . Wanted
  errors = Errors . Seq.singleton . Joined

-- -----------------------------------------------------------------------------
-- Analysis types

-- | The 'Analysis' type is a result of running the parser
-- statically. It provides some information about the kind of parse
-- that would be attempted and is thus useful for error messages or
-- manual documentation.
data Analysis
  = Succeeding
  | Wanting     String
  | Branching   (Seq.Seq Analysis)
  | Joining     Analysis
  | Defaulting  String Analysis
  | Documenting String Analysis
  deriving ( Eq, Show )

-- | Get each environment varaible the parser wants. This will include
-- ones that may be optional due to default values.
references :: Analysis -> [String]
references = toList . foldAnalysis where
  foldAnalysis Succeeding          = Seq.empty
  foldAnalysis (Wanting key)       = Seq.singleton key
  foldAnalysis (Branching ds)      = foldMap foldAnalysis ds
  foldAnalysis (Joining d)         = foldAnalysis d
  foldAnalysis (Defaulting _ d)    = foldAnalysis d
  foldAnalysis (Documenting doc d) = foldAnalysis d

data Df a = Df { runDf :: Analysis } deriving Functor

instance Applicative Df where
  pure _ = Df Succeeding
  Df (Branching dfs) <*> Df (Branching dxs) = Df (Branching $ dfs <> dxs)
  Df (Branching dfs) <*> Df dx              = Df (Branching $ dfs |> dx)
  Df df              <*> Df (Branching dxs) = Df (Branching $ df <| dxs)
  Df df              <*> Df dx              = Df (Branching $ Seq.fromList [df, dx])

instance Cls.HasEnv Df where
  getEnv key = Df (Wanting key)

instance Cls.Env Df where
  joinFailure (Df dep)   = Df (Joining dep)
  def a sho (Df dep)     = Df (Defaulting (sho a) dep)

-- -----------------------------------------------------------------------------
-- Parser types

-- | The generic environment 'Parser'. This type is used to specify
-- the structure of a configuration which can be read from the
-- environment. Later that structure can either be examined using
-- 'analyze', tested using 'test', or performed on the actual
-- environment using 'run'.
data Parser a = Parser
  { run'     :: Compose IO (Miss Errors) a
  , test'    :: Compose ((->) (Map.Map String String)) (Miss Errors) a
  , analyze' :: Df a
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

-- | Run a 'Parser' in 'IO' using the actual environment.
run :: Parser a -> IO (Either Errors a)
run = fmap toEither . getCompose . run'

-- | Run a 'Parser' purely using a 'Map.Map' to simulate the
-- environment.
test :: Parser a -> Map.Map String String -> Either Errors a
test = fmap toEither . getCompose . test'

-- | Run a completely pure, static analysis of the 'Parser' that can
-- be used to generate helpful documentation.
analyze :: Parser a -> Analysis
analyze = runDf . analyze'

-- | Pull a value from the environment.
get :: FE.FromEnv a => String -> Parser a
get = FE.fromEnv . Cls.getEnv

-- | Assign a default value to a 'Parser'. If the parser should fail at
-- runtime the default value will be returned instead. The value must
-- be showable in order to provide documentation.
def :: Show a => a -> Parser a -> Parser a
def a = Cls.def a show

-- | Assign a default value to a 'Parser'. This is identical to 'def'
-- but does not require the default value has a 'Show'
-- instance---instead a constant descriptive string should be passed
-- directly.
def' :: a -> String -> Parser a -> Parser a
def' a str = Cls.def a (const $ "{" ++ str ++ "}")

-- | Assign documentation to a branch of the 'Parser'. This can be
-- later retrieved in the 'Analysis' type.
doc :: String -> Parser a -> Parser a
doc = Cls.doc

-- | Pull a string from the environment and interpret it as a
-- JSON-serializable type.
json :: Ae.FromJSON a => String -> Parser a
json = getParse (Ae.parseEither Ae.parseJSON)

-- | Pull a value from the environment and attempt to parse it into
-- some other type. Failures will be 'Joined' into the result.
getParse :: FE.FromEnv a => (a -> Either String b) -> String -> Parser b
getParse parse key = Cls.joinFailure $ fmap parse $ get key
