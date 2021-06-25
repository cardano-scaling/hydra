-- | Tests the hydra node as a unit, while providing full control over the
-- Network, Chain and API components. This allows convenient ingestion of events
-- and assertion on observable effects.
module Hydra.NodeSpec where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar, newTVarIO, readTVarIO)
import Hydra.Chain (Chain (..))
import Hydra.HeadLogic (
  Environment (..),
  Event (NetworkEvent),
  HeadParameters (..),
  HeadState (..),
  HydraMessage (..),
  Snapshot (..),
  SnapshotStrategy (SnapshotAfterEachTx),
  openStateWith,
 )
import Hydra.Ledger (deriveParty, generateKey, sign)
import Hydra.Ledger.Builder (aValidTx, utxoRef)
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Logging (nullTracer)
import Hydra.Network (Network (..))
import Hydra.Node (EventQueue (isEmpty), HydraNode (..), createEventQueue, createHydraHead, putEvent, queryHeadState, stepHydraNode)
import Test.Hspec (
  Spec,
  describe,
  it,
  pending,
 )
import Test.Util (failure, shouldBe, shouldRunInSim)

spec :: Spec
spec = describe "Unit tests for a single hydra node" $ do
  it "sends transactions received from client onto the network" pending

  it "does not forward invalid transactions received from client" pending

  it "does not broadcast reqTx given new transaction is invalid" pending

  it "does not request snapshot when already having one in flight" $
    shouldRunInSim $ do
      let env = envFor 1
          p = party env
          s0 = HeadState (HeadParameters 3 [p]) $ openStateWith mempty

      -- In the first processing loop, all ReqTx will lead to a ReqSn
      s1 <-
        expectingOnNetwork
          env
          s0
          [ReqTx (aValidTx 1), ReqTx (aValidTx 2)]
          [ReqSn p 1 [aValidTx 1], ReqSn p 1 [aValidTx 2, aValidTx 1]]

      -- In the second processing loop, the (own) ReqSn will be acked, but no
      -- new snapshots are requested
      let signature = sign 1 $ Snapshot 1 (utxoRef 1) [aValidTx 1]
      void $
        expectingOnNetwork
          env
          s1
          [ReqSn p 1 [aValidTx 1], ReqTx (aValidTx 3)]
          [AckSn p signature 1]

-- NOTE(SN): many things could be re-used here if we would want to have
-- 'expectingOnChain' etc
expectingOnNetwork ::
  (HasCallStack, MonadSTM m, MonadThrow m, MonadAsync m, MonadTimer m) =>
  Environment ->
  HeadState SimpleTx ->
  [HydraMessage SimpleTx] ->
  [HydraMessage SimpleTx] ->
  m (HeadState SimpleTx)
expectingOnNetwork env headState events expected = do
  eq <- primedQueue
  hh <- createHydraHead headState simpleLedger
  actualVar <- newTVarIO []
  let node =
        HydraNode
          { eq
          , hh
          , hn = recordNetworkMessages actualVar
          , oc = notExpectedChain
          , sendResponse = ignoreResponses
          , env
          }
  newState <- runToCompletion node
  actual <- reverse <$> readTVarIO actualVar
  actual `shouldBe` expected
  pure newState
 where
  primedQueue = do
    q <- createEventQueue
    mapM_ (putEvent q . NetworkEvent) events
    pure q

  recordNetworkMessages tvar = Network{broadcast = \msg -> atomically $ modifyTVar tvar (msg :)}

  notExpectedChain = Chain{postTx = \tx -> failure $ "unexpected postTx: " <> show tx}

  ignoreResponses _ = pure ()

  runToCompletion node@HydraNode{eq, hh} = do
    stepHydraNode nullTracer node
    isEmpty eq >>= \case
      True -> atomically (queryHeadState hh)
      False -> runToCompletion node

envFor :: Integer -> Environment
envFor n =
  let signingKey = generateKey n
      party = deriveParty signingKey
   in Environment
        { party
        , signingKey
        , otherParties = mempty
        , snapshotStrategy = SnapshotAfterEachTx
        }
