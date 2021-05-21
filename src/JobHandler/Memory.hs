module JobHandler.Memory where

import Core
import RIO
import qualified JobHandler
import qualified RIO.Map as Map
import qualified Control.Concurrent.STM as STM

data State
  = State
      { jobs :: Map BuildNumber JobHandler.Job
      , nextBuild :: Int
      }

createService :: IO JobHandler.Service
createService = do
  state <- STM.newTVarIO State
    { jobs = mempty
    , nextBuild = 1
    }

  pure JobHandler.Service
    { queueJob = \pipeline -> STM.atomically do
        STM.stateTVar state $ queueJob_ pipeline
    , findJob = \number -> STM.atomically do
        s <- STM.readTVar state
        pure $ findJob_ number s
    , dispatchCmd = pure undefined
    , processMsg = \_ -> undefined
    }

queueJob_ :: Pipeline -> State -> (BuildNumber, State)
queueJob_ pipeline state =
  (number, updatedState)
  where
    number = BuildNumber state.nextBuild
    job = JobHandler.Job
      { pipeline = pipeline
      , state = JobHandler.JobQueued
      }
    updatedState =
      state
        { jobs = Map.insert number job state.jobs
        , nextBuild = state.nextBuild + 1
        }

findJob_ :: BuildNumber -> State -> Maybe JobHandler.Job
findJob_ number state =
  Map.lookup number state.jobs
