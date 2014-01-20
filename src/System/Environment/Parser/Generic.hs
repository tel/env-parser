-- |
-- Module      : System.Environment.Parser.Internal
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
module System.Environment.Parser.Generic (

  FromEnv (parseEnv), HasEnv (..), Env (..),
  
  ) where

import           System.Environment.Parser.Class
import           System.Environment.Parser.FromEnv

