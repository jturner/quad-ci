module JobHandler where

import qualified Agent
import Core
import RIO

data Job = Job
  { pipeline :: Pipeline,
    state :: JobState
  }
  deriving (Eq, Show)

data JobState
  = JobQueued
  | JobAssigned
  | JobScheduled Build
  deriving (Eq, Show)

data Service = Service
  { queueJob :: Pipeline -> IO BuildNumber,
    dispatchCmd :: IO (Maybe Agent.Cmd),
    processMsg :: Agent.Msg -> IO (),
    findJob :: BuildNumber -> IO (Maybe Job)
  }

data CommitInfo = CommitInfo
  { sha :: Text,
    repo :: Text
  }
  deriving (Eq, Show)
