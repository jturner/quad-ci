module Agent where

import Core
import RIO
import qualified Codec.Serialise as Serialise
import qualified Network.HTTP.Simple as HTTP
import qualified Runner

data Cmd
  = StartBuild BuildNumber Pipeline
  deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
  = LogCollected BuildNumber Log
  | BuildUpdated BuildNumber Build
  deriving (Eq, Show, Generic, Serialise.Serialise)

data Config
  = Config
      { endpoint :: String
      }

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
  endpoint <- HTTP.parseRequest config.endpoint
  let req = endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/pull"

  res <- HTTP.httpLBS req
  let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd
  traverse_ (runCommand runner) cmd

  threadDelay (1 * 1000 * 1000)

runCommand :: Runner.Service -> Cmd -> IO ()
runCommand runner = \case
  StartBuild number pipeline -> do
    let hooks = Runner.Hooks
          { logCollected = traceShowIO
          }

    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build
