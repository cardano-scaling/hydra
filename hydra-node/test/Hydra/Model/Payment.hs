{-# LANGUAGE TypeApplications #-}
-- | A simplistic type of transactions useful for modelling purpose.
-- a `Payment` is a simple transaction type that moves some amount of ADAs between
-- to `CardanoSigningKey`.
module Hydra.Model.Payment where


import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (pairs)
import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (TxSeq))
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadAsync (Async, async, cancel)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadSTM (
  MonadLabelledSTM,
  labelTQueueIO,
  labelTVarIO,
  modifyTVar,
  newTQueue,
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
import Control.Monad.Class.MonadTimer (timeout)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.API.ClientInput (ClientInput)
import qualified Hydra.API.ClientInput as Input
import Hydra.API.ServerOutput (ServerOutput (Committed, GetUTxOResponse, SnapshotConfirmed))
import qualified Hydra.API.ServerOutput as Output
import Hydra.BehaviorSpec (
  ConnectToChain (..),
  TestHydraNode (..),
  createHydraNode,
  createTestHydraNode,
  shortLabel,
  waitMatch,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (Chain (..), HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.Chain.Direct.Handlers (ChainSyncHandler, DirectChainLog, SubmitTx, chainSyncHandler, mkChain, onRollForward)
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.State (ChainContext, ChainStateAt (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle)
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Committed (),
  Environment (party),
  Event (NetworkEvent),
  HeadState (..),
  PendingCommits,
  defaultTTL,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genSigningKey, genTxIn, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  HydraNode (..),
  NodeState (NodeState),
  chainCallback,
  createNodeState,
  modifyHeadState,
  putEvent,
  queryHeadState,
  runHydraNode,
 )
import Hydra.Party (Party (..), deriveParty)
import qualified Hydra.Snapshot as Snapshot
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (choose, counterexample, elements, frequency, resize, sized, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), LookUp, RunModel (..), StateModel (..), Var)
import qualified Prelude

newtype CardanoSigningKey = CardanoSigningKey {signingKey :: SigningKey PaymentKey}

instance Show CardanoSigningKey where
  show CardanoSigningKey{signingKey} =
    show . mkVkAddress @Era testNetworkId . getVerificationKey $ signingKey

-- NOTE: We need this orphan instance in order to lookup keys in lists.
instance Eq CardanoSigningKey where
  CardanoSigningKey (PaymentSigningKey skd) == CardanoSigningKey (PaymentSigningKey skd') = skd == skd'

instance ToJSON CardanoSigningKey where
  toJSON = error "don't use"

instance FromJSON CardanoSigningKey where
  parseJSON = error "don't use"

instance Arbitrary Value where
  arbitrary = genAdaValue

instance Arbitrary CardanoSigningKey where
  arbitrary = CardanoSigningKey . snd <$> genKeyPair

-- | A single Ada-payment only transaction in our model.
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Payment where
  -- NOTE: We display derived addresses instead of raw signing keys in order to help troubleshooting
  -- tests failures or errors.
  show Payment{from, to, value} =
    "Payment { from = " <> show from
      <> ", to = "
      <> show to
      <> ", value = "
      <> show value
      <> " }"

instance Arbitrary Payment where
  arbitrary = error "don't use"

instance ToCBOR Payment where
  toCBOR = error "don't use"

instance FromCBOR Payment where
  fromCBOR = error "don't use"

-- | Making `Payment` an instance of `IsTx` allows us to use it with `HeadLogic'`s messages.
instance IsTx Payment where
  type TxIdType Payment = Int
  type UTxOType Payment = [(CardanoSigningKey, Value)]
  type ValueType Payment = Value
  txId = error "undefined"
  balance = foldMap snd
  hashUTxO = encodeUtf8 . show @Text

applyTx :: UTxOType Payment -> Payment -> UTxOType Payment
applyTx utxo Payment{from, to, value} =
  (to, value) : List.delete (from, value) utxo

genAdaValue :: Gen Value
genAdaValue = lovelaceToValue . fromInteger <$> choose (1, 10000000000)
