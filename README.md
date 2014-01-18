
# Configuration from the Environment

`env-parser` is a small library for configuring programs based on information
from the environment. It's goals and design are similar to that of `cmdargs` or
`optparse-applicative` but aimed less at command-line programs which are
executed and configured manually and more for automatically managed programs
such as those that might be run via Heroku or Runit/daemontools.

`env-parser` focuses on making ENV-configured programs both easy to build,
declarative, self-documenting, and easy to test. It also provides a number of
built-in parsers which make it easy to build programs that depend upon
environment variables in a predictable, conventional fashion.

```haskell
import qualified System.Environment.Parser as Env

data Config = Config
  { dbConnInfo  :: Env.DBConnect
  , dbConnCount :: Int
  , dbConnTime  :: NominalDiffTime
  , twitterCred :: Oauth.Cred Oauth.Permanent
  }
  deriving ( Eq, Show )

envConfig :: Env.Parser Config
envConfig = Config
  <$> Env.get "DATABASE_URL"
  <*> (Env.get "DATABASE_CONNECTION_LIMIT"   <|> pure 3)
  <*> (Env.get "DATABASE_CONNECTION_TIMEOUT" <|> pure 10)
  <*> envCred

  where
    envCred = Oauth.Cred
      <$> Env.get "TWITTER_PERMANENT_TOKEN"
      <*> Env.get "TWITTER_PERMANENT_SECRET"
      <*> ( Oauth.Token 
              <$> Env.get "TWITTER_CLIENT_TOKEN"
              <*> Env.get "TWITTER_CLIENT_SECRET" )

main :: IO ()
main = do
  x <- runEnvParser envConfig
  case x of
    Left errs -> do print errs
                    showHelp envConfig
    Right c0  -> runProgram c0
```
