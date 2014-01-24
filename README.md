
# Configuration from the Environment

`env-parser` is a small library for configuring programs based on information
from the environment. It's goals and design are similar to that of `cmdargs` or
`optparse-applicative` but aimed at automatically managed programs such as those
that might be run via Heroku or Runit/daemontools.

`env-parser` intentionally sacrifices power for comprehensibility---the primary
interface, `Parser`, implements only `Applicative`. This provides better runtime
error messages and automatically generated static help using parser annotations.
It also expresses a principle of simplicity in configuration: arbitrary uses of
`Monad` or even `Alternative` can lead to opaque failures prior to a program
even beginning to run.
