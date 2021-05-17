module Hydra.FireForgetSpec where

import Cardano.Prelude hiding (atomically, concurrently)

import Control.Monad.Class.MonadAsync (concurrently)
import Control.Monad.Class.MonadSTM (MonadSTM (..), TVar, readTVar, writeTVar)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import Network.TypedProtocol.Channel (createConnectedChannels)
import Network.TypedProtocol.Driver.Simple (runPeer)
import Network.TypedProtocol.FireForget.Client (FireForgetClient (..), fireForgetClientPeer)
import Network.TypedProtocol.FireForget.Server (FireForgetServer (..), fireForgetServerPeer)
import Network.TypedProtocol.FireForget.Type (codecFireForget)
import Test.Hspec

spec :: Spec
spec = describe "Fire-Forget Ouroboros Protocol" $ do
  it "client can send 'Hail Hydra!' to server" $ do
    let action :: IOSim s (Text, ())
        action = do
          (channelA, channelB) <- createConnectedChannels
          server <- newServer
          concurrently
            (runPeer nullTracer codecFireForget channelA $ fireForgetServerPeer server)
            (runPeer nullTracer codecFireForget channelB $ fireForgetClientPeer client)
    let (resultServer, _) = runSimOrThrow action
    resultServer `shouldBe` "Hail Hydra!"

client :: Applicative m => FireForgetClient Text m ()
client =
  SendMsg ("Hail Hydra!" :: Text) $ pure $ SendDone $ pure ()

newServer :: forall m. MonadSTM m => m (FireForgetServer Text m Text)
newServer = do
  tvar <- newTVarIO ""
  pure $ server tvar
 where
  server :: TVar m Text -> FireForgetServer Text m Text
  server tvar =
    FireForgetServer
      { recvMsg = \msg -> do
          atomically (writeTVar tvar msg)
          pure (server tvar)
      , recvMsgDone =
          atomically (readTVar tvar)
      }
