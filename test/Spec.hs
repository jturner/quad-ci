module Main where

import RIO
import Core
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified Docker

makeStep :: Text -> Text -> [Text] -> Step
maekStep name image commands
  = Step
      { name = StepName name
      , image = Docker.Image image
      , commands = NonEmpty.Partial.fromList commands
      }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = NonEmpty.Partial.fromList steps }

testPipeline :: Pipeline
testPipeline = makePipeline
  [ makeStep "First step" "ubuntu" ["date"]
  , makeStep "Second step" "ubuntu" ["uname -r"]
  ]

testBuild :: Build
testBuild = build
  { pipeline = testPipeline
  , state = BuildReady
  , completedSteps = mempty
  }

main :: IO ()
main = pure ()
