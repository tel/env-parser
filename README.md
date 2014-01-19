
# Configuration from the Environment

`env-parser` is a small library for configuring programs based on information
from the environment. It's goals and design are similar to that of `cmdargs` or
`optparse-applicative` but aimed less at command-line programs which are
executed and configured manually and more for automatically managed programs
such as those that might be run via Heroku or Runit/daemontools.

`env-parser` focuses on making environment-configured programs both easy to
build, declarative, self-documenting, and easy to test. It also provides a
number of built-in parsers which make it easy to build programs that depend
upon environment variables in a predictable, conventional fashion.

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
    envCred :: Env.Parser (Oauth.Cred Oauth.Permanent)
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

## Why only implement `Alternative`?

Most parsers of this style instantiate both `Alternative` and `MonadPlus`
providing significantly more flexibility in the kinds of parses which can be
represented. `env-parser` chooses not to do this in order to allow deeper
analysis of the kinds of environmental contexts a program requires.

By allowing only as much power as `Alternative` it's possible to determine
directly from the parser itself all of the environment variables the program
depends upon and present this to the user. We also allow arbitrary validation
and transformation to occur via `liftFailure` (a weakened `join`) which provides
a lot of the benefit of a `Monad` instance without sacrificing
comprehensibility.

This essentially maintains a divide between your actual program and your
configuration information--the former can have complex behavior but the latter
should always be easy to understand. This will lead to more comprehensible
errors and configuration documentation.
