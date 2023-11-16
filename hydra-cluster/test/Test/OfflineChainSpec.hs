{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.OfflineChainSpec where
import Hydra.Prelude
import Test.Hydra.Prelude
import Hydra.Chain (PostChainTx, ChainEvent)
import Hydra.Logging (showLogsOnFailure, Tracer)
import Hydra.Chain.Offline (withOfflineChain)
import Hydra.Options (defaultContestationPeriod, OfflineConfig, initialUTxOFile)
import Hydra.Party (deriveParty)
import Hydra.Options (defaultOfflineConfig)
import Hydra.Chain (HeadParameters(HeadParameters, contestationPeriod, parties))
import Hydra.Cluster.Util (keysFor, seedInitialUTxOFromOffline)
import Hydra.Cluster.Fixture (Actor(Alice), aliceSk)
import Hydra.Chain (initHistory)
import Hydra.Chain.Direct.State (initialChainState)
import Control.Concurrent.STM (newEmptyTMVarIO, putTMVar, takeTMVar)
import Hydra.Chain (Chain(Chain, postTx))

--TODO(Elaine): replace with some offlinechainlog?
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Party (Party)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Chain (OnChainTx(OnCommitTx, party, committed))

import Test.ChainSpec (hasInitTxWith, observesInTime)
import Hydra.Ledger (IsTx(UTxOType))
import Hydra.Chain (ChainEvent(observedTx))
import Hydra.Chain (PostChainTx(AbortTx))
import Hydra.Chain (OnChainTx(OnAbortTx))
import Hydra.HeadId(HeadId(HeadId))
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)


data OfflineChainTest tx m = OfflineChainTest
  { postTx :: PostChainTx tx -> m ()
  , waitCallback :: m (ChainEvent tx)
  }

withOfflineChainTest :: Tracer IO DirectChainLog -> OfflineConfig
                  -> Party
                  -> (OfflineChainTest Tx IO -> IO b)
                  -> IO b
withOfflineChainTest tracer offlineConfig party action = do
  eventMVar <- newEmptyTMVarIO

  let callback event = atomically $ putTMVar eventMVar event
      globals = undefined
      contestationPeriod = defaultContestationPeriod
  withOfflineChain tracer offlineConfig globals (HeadId "HeadId") party contestationPeriod  (initHistory initialChainState) callback $ \Chain{postTx} -> do
    action
      OfflineChainTest
        { postTx 
        , waitCallback = atomically $ takeTMVar eventMVar
        }

spec :: Spec
spec = around showLogsOnFailure $ do
    it "can init and abort an offline head given nothing has been externally comitted" $ \tracer -> do
        withTempDir "hydra-cluster" $ \tmp -> do
            (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
            -- let aliceHydraKey = generateSigningKey . show $ (1::Integer) --based on EndToEnd.hs
            let aliceParty = deriveParty aliceSk
            --TODO(Elaine): i think we have to make this relative see readConfigFile
            initialUTxO <- readJsonFileThrow (parseJSON @(UTxOType Tx)) $ initialUTxOFile defaultOfflineConfig

            seedInitialUTxOFromOffline initialUTxO tmp
            
            withOfflineChainTest tracer defaultOfflineConfig aliceParty $ \OfflineChainTest{postTx, waitCallback} -> do
                -- postTx $ InitTx
                --     { headParameters = HeadParameters
                --         { contestationPeriod = defaultContestationPeriod
                --         , parties = [aliceParty]
                --         }
                --     }
                -- we should automatically have an init, commit, event play because withOfflineChain calls initializeStateIfOffline
                -- but withDirectChain doesnt do this so it makes me wonder if we should revert the change


                participants <- loadParticipants [Alice]

                event <- waitCallback -- because we've got a tmvar the stuff in withOfflineChain should block until we've read out the remaining events
                hasInitTxWith defaultContestationPeriod [aliceParty] participants $ observedTx event

                event' <- waitCallback 
                -- event' `shouldBe` Observation { observedTx = OnCommitTx { party = aliceParty, committed = initialUTxO } }
                observedTx event' `observesInTime` OnCommitTx { party = aliceParty, committed = initialUTxO }

                postTx $ AbortTx {-TODO(Elaine): what are the semantics of this again -} mempty
                event'' <- waitCallback
                -- need to make observesintime etc generic to get timeout on this
                observedTx event'' `observesInTime` OnAbortTx



                
                pure ()
            pure ()
    
    pure ()

-- hasInitTxWith :: ContestationPeriod -> Party -> ChainEvent Tx -> Expectation
-- hasInitTxWith expectedContestationPeriod expectedParty = \case
--     OnInitTx {contestationPeriod, parties} -> pure ()
--     _ -> expectationFailure $ "expected InitTx, got " <> show event
