module Hydra.API.ServerSpec where

import Cardano.Prelude hiding (atomically, threadDelay)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Class.MonadSTM (TQueue, atomically, newTQueue, readTQueue, tryReadTQueue, writeTQueue)
import Control.Monad.Class.MonadTimer (threadDelay)
import qualified Data.Text as Text
import Hydra.API.Server (APIServerLog, withAPIServer)
import Hydra.HeadLogic (ClientResponse (..))
import Hydra.Ledger.Mock (MockTx)
import Hydra.Logging (Tracer (..))
import Network.WebSockets (runClient)
import Network.WebSockets.Connection (receiveData)
import Test.Hspec
import Test.Util (failAfter)

spec :: Spec
spec = describe "API Server" $ do
  it "sends response to all connected clients" $ do
    queue <- atomically newTQueue
    failAfter 5 $
      withAPIServer @MockTx "127.0.0.1" 54321 showStdoutTracer noop $ \response -> do
        threadDelay 0.5
        withAsync (concurrently_ (testClient queue) (testClient queue)) $ \_ -> do
          threadDelay 0.5
          response ReadyToCommit

          atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [ReadyToCommit, ReadyToCommit]
          atomically (tryReadTQueue queue) `shouldReturn` Nothing

showStdoutTracer :: Tracer IO APIServerLog
showStdoutTracer = Tracer print

noop :: Applicative m => a -> m ()
noop = const $ pure ()

testClient :: HasCallStack => TQueue IO (ClientResponse MockTx) -> IO ()
testClient queue =
  runClient
    "127.0.0.1"
    54321
    "/"
    ( \cnx -> do
        msg <- receiveData cnx
        case readMaybe (Text.unpack msg) of
          Just resp -> atomically (writeTQueue queue resp)
          Nothing -> expectationFailure (show $ "Failed to decode message " <> msg)
    )
