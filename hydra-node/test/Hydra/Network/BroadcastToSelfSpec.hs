module Hydra.Network.BroadcastToSelfSpec where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.IOSim (runSimOrThrow)
import Hydra.Network (Network (..))
import Hydra.Network.BroadcastToSelf (withBroadcastToSelf)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Broadcast To Self" $ do
  it "broadcasted messages are sent back to the sender" $ do
    let received = runSimOrThrow $ do
          receivedMessages <- newTVarIO ([] :: [Integer])

          let receive msg = atomically $ modifyTVar' receivedMessages (msg :)
              noopNetwork _cb action = action $ Network{broadcast = const $ pure ()}

          withBroadcastToSelf noopNetwork receive $ \Network{broadcast} -> do
            broadcast 42

          readTVarIO receivedMessages

    received `shouldBe` [42]
