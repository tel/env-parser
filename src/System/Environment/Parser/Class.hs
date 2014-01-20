{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : System.Environment.Parser.Class
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
module System.Environment.Parser.Class where

import           Control.Applicative
import qualified Control.Exception              as E
import           Data.Functor.Compose
import qualified Data.Map                       as Map
import           Data.Monoid
import qualified System.Environment             as Env
import           System.Environment.Parser.Miss

-- -----------------------------------------------------------------------------
-- The HasEnv class

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

instance (HasEnv f, Applicative f) => HasEnv (Compose IO f) where
  getEnv key = Compose $ maybe (getEnv key) pure <$> getEnvSafe key

instance (HasEnv f, Applicative f)
         => HasEnv (Compose ((->) (Map.Map String String)) f) where
  getEnv key = Compose $ maybe (getEnv key) pure . Map.lookup key

-- ----------------------------------------------------------------------------
-- Satisfiable class

class Monoid a => Satisfiable a where
  wants  :: String -> a
  errors :: String -> a

-- ----------------------------------------------------------------------------
-- The Env class

class Applicative r => Env r where
  -- | Express that a parse has failed
  joinFailure :: r (Either String a) -> r a

  -- | Replace a result with a default if necessary
  def :: a -> (a -> String) -> r a -> r a

instance Satisfiable e => HasEnv (Miss e) where
  getEnv key = Miss (wants key)

instance Satisfiable e => Env (Miss e) where
  joinFailure (Miss er) = Miss er
  joinFailure (Got (Left er)) = Miss (errors er)
  joinFailure (Got (Right a)) = Got a

  def a _ (Miss _) = Got a
  def _ _ (Got a)  = Got a

instance (Applicative f, Env g) => Env (Compose f g) where
  joinFailure = Compose . fmap joinFailure . getCompose
  def a sho   = Compose . fmap (def a sho) . getCompose

-- | Default a 'Show'-able type.
defShow :: (Show a, Env r) => a -> r a -> r a
defShow a = def a show

-- | Default a type with a constant string.
defNoShow :: Env r => a -> String -> r a -> r a
defNoShow a str = def a (const str)
