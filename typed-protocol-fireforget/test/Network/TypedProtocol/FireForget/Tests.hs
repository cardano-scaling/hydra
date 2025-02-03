module Network.TypedProtocol.FireForget.Tests where

import Control.Concurrent.Class.MonadSTM (MonadSTM (atomically), TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Class.MonadAsync (concurrently)
import Control.Tracer (nullTracer)
import Data.Text (Text)
import Network.TypedProtocol.Channel (createConnectedChannels)
import Network.TypedProtocol.Driver.Simple (runPeer)
import Network.TypedProtocol.FireForget.Client (FireForgetClient (SendDone, SendMsg), fireForgetClientPeer)
import Network.TypedProtocol.FireForget.Codec (codecFireForget)
import Network.TypedProtocol.FireForget.Server (FireForgetServer (FireForgetServer, recvMsg, recvMsgDone), fireForgetServerPeer)
import Test.Hspec.Core.Spec (Spec, it)
import Test.Util (shouldBe, shouldRunInSim)

spec :: Spec
spec =
  it "client can send 'Hail Hydra!' to server" $ do
    ((res, _), _) <- shouldRunInSim $ do
      (channelA, channelB) <- createConnectedChannels
      server <- newServer
      concurrently
        (runPeer nullTracer codecFireForget channelA $ fireForgetServerPeer server)
        (runPeer nullTracer codecFireForget channelB $ fireForgetClientPeer client)
    res `shouldBe` "Hail Hydra!"

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
          readTVarIO tvar
      }
