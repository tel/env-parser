
-- |
-- Module      : System.Environment.Parser.Database
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Heroku-style database connection URL parsing.
module System.Environment.Parser.Database (

  DBConnection (..), Provider (..), providerString

 ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text               as At
import qualified Data.ByteString                    as S
import qualified Data.ByteString.Char8              as S8
import qualified Data.Map                           as Map
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Network.HTTP.Types                 as Ht
import qualified Network.URI                        as URI
import           System.Environment.Parser.FromEnv

----------------------------------------------------------------------------
-- Heroku-style DB urls

-- | Provider indicates what kind of database provider scheme this URL
-- refers to. This provides a small amount of convenience around URL
-- detection, but if detection fails use 'providerName' to extract the raw
-- string.
data Provider = Postgres S.ByteString
              | MySQL    S.ByteString
              | AMQP     S.ByteString
              | HTTP     S.ByteString
              | Other    S.ByteString
  deriving ( Eq, Ord, Show, Read )

providerString :: Provider -> S.ByteString
providerString p = case p of
  Postgres s -> s
  MySQL    s -> s
  AMQP     s -> s
  HTTP     s -> s
  Other    s -> s

-- | A type representing the information that can be parsed from
-- a URL-style database configuration. For example consider the following
-- URLs
--
-- > postgres://user3123:passkja83kd8@ec2-117-21-174-214.compute-1.amazonaws.com:6212/db982398
-- > mysql://adffdadf2341:adf4234@us-cdbr-east.cleardb.com/heroku_db?reconnect=true
-- > mysql2://adffdadf2341:adf4234@us-cdbr-east.cleardb.com/heroku_db?reconnect=true
-- > http://user:pass@instance.ip/resourceid
-- > amqp://user:pass@ec2.clustername.cloudamqp.com/vhost
--
-- We assume that all the information in these URLs is provided---if
-- a username or password is unnecessary then it must be passed as an empty
-- string and not omitted.
--
data DBConnection = DBConnection
  { provider :: Provider
  , host     :: S.ByteString
  , username :: S.ByteString
  , password :: S.ByteString
  , port     :: Int
  , location :: S.ByteString
  , params   :: Map.Map S.ByteString S.ByteString
  }
  deriving ( Eq, Ord, Show, Read )

instance FromEnv DBConnection where
  parseEnv = tryParse

tryParse :: String -> Either String DBConnection
tryParse s = do
  uri  <- e "invalid URI format" $ URI.parseAbsoluteURI s
  auth <- e "URI authority segment missing" $ URI.uriAuthority uri
  port <- parsePort (URI.uriPort auth)
  (username, password) <- parseUserPw (URI.uriUserInfo auth)

  return DBConnection
    { provider = guessProvider (URI.uriScheme uri)
    , host     = S8.pack (URI.uriRegName auth)
    , username = username
    , password = password
    , port     = port
    , location = S8.pack $ drop 1 $ URI.uriPath uri
    , params   = makeQueryMap (URI.uriQuery uri)
    }

  where

    guessProvider :: String -> Provider
    guessProvider x = case x of
      "postgres:" -> Postgres bs
      "mysql:"    -> MySQL    bs
      "mysql2:"   -> MySQL    bs
      "http:"     -> HTTP     bs
      "amqp:"     -> AMQP     bs
      _           -> Other    bs
      where bs = S8.pack s

    parsePort :: String -> Either String Int
    parsePort = At.parseOnly (At.char ':' *> At.decimal) . T.pack

    parseUserPw :: String -> Either String (S.ByteString, S.ByteString)
    parseUserPw = At.parseOnly ( (,) <$> (TE.encodeUtf8 <$> At.takeTill (==':') <* At.char ':')
                                     <*> (TE.encodeUtf8 <$> At.takeTill (=='@') <* At.char '@') )
                . T.pack

    makeQueryMap :: String -> Map.Map S.ByteString S.ByteString
    makeQueryMap = Map.fromList . Ht.parseSimpleQuery . S8.pack

-- -----------------------------------------------------------------------------
-- Utilities

e :: String -> Maybe a -> Either String a
e s Nothing  = Left s
e _ (Just a) = Right a
