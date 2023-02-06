{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provide infrastructure-independent "handlers" for posting transactions and following the chain.
--
-- This module encapsulates the transformation logic between cardano transactions and `HydraNode` abstractions
-- `PostChainTx` and `OnChainTx`, and maintainance of on-chain relevant state.
module Hydra.Chain.Direct.Handlers where

import Hydra.Prelude

import Cardano.Ledger.Babbage.Tx (ValidatedTx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (SupportsSegWit (fromTxSeq))
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Class.MonadSTM (throwSTM)
import Data.Sequence.Strict (StrictSeq)
import Hydra.Cardano.Api (
  ChainPoint (..),
  ConsensusMode (CardanoMode),
  LedgerEra,
  Tx,
  TxId,
  chainPointToSlotNo,
  fromConsensusPointInMode,
  fromLedgerTx,
  getTxBody,
  getTxId,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainEvent (..),
  ChainStateType,
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct.State (
  ChainContext (contestationPeriod),
  ChainState (Closed, Idle, Initial, Open),
  ChainStateAt (..),
  abort,
  chainSlotFromPoint,
  close,
  collect,
  commit,
  contest,
  fanout,
  getKnownUTxO,
  initialize,
  observeSomeTx,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Hydra.Chain.Direct.Util (Block)
import Hydra.Chain.Direct.Wallet (
  ErrCoverFee (..),
  TinyWallet (..),
  TinyWalletLog,
 )
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Logging (Tracer, traceWith)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..))
import Ouroboros.Network.Block (Point (..), blockPoint)
import Plutus.Orphans ()
import System.IO.Error (userError)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

-- * Posting Transactions

-- | A callback used to actually submit a transaction to the chain.
type SubmitTx m = ValidatedTx LedgerEra -> m ()

-- | A way to acquire a 'TimeHandle'
type GetTimeHandle m = m TimeHandle

-- | Create a `Chain` component for posting "real" cardano transactions.
--
-- This component does not actually interact with a cardano-node, but creates
-- cardano transactions from `PostChainTx` transactions emitted by a
-- `HydraNode`, balancing and signing them using given `TinyWallet`, before
-- handing it off to the given 'SubmitTx' callback.
--
-- NOTE: Given the constraints on `m` this function should work within `IOSim` and does not
-- require any actual `IO` to happen which makes it highly suitable for simulations and testing.
mkChain ::
  (MonadSTM m, MonadTimer m, MonadThrow (STM m)) =>
  Tracer m DirectChainLog ->
  -- | Means to acquire a new 'TimeHandle'.
  GetTimeHandle m ->
  TinyWallet m ->
  ChainContext ->
  SubmitTx m ->
  Chain Tx m
mkChain tracer queryTimeHandle wallet ctx submitTx =
  Chain
    { postTx = \chainState tx -> do
        traceWith tracer $ ToPost{toPost = tx}
        timeHandle <- queryTimeHandle
        vtx <-
          -- FIXME (MB): cardano keys should really not be here (as this
          -- point they are in the 'chainState' stored in the 'ChainContext')
          -- . They are only required for the init transaction and ought to
          -- come from the _client_ and be part of the init request
          -- altogether. This goes in the direction of 'dynamic heads' where
          -- participants aren't known upfront but provided via the API.
          -- Ultimately, an init request from a client would contain all the
          -- details needed to establish connection to the other peers and
          -- to bootstrap the init transaction. For now, we bear with it and
          -- keep the static keys in context.
          atomically (prepareTxToPost timeHandle wallet ctx chainState tx)
            >>= finalizeTx wallet ctx chainState . toLedgerTx
        submitTx vtx
    }

-- | Balance and sign the given partial transaction.
finalizeTx ::
  (MonadThrow m) =>
  TinyWallet m ->
  ChainContext ->
  ChainStateType Tx ->
  ValidatedTx LedgerEra ->
  m (ValidatedTx LedgerEra)
finalizeTx TinyWallet{sign, coverFee} ctx ChainStateAt{chainState} partialTx = do
  let headUTxO = getKnownUTxO ctx <> getKnownUTxO chainState
  coverFee (Ledger.unUTxO $ toLedgerUTxO headUTxO) partialTx >>= \case
    Left ErrNoFuelUTxOFound ->
      throwIO (NoFuelUTXOFound :: PostTxError Tx)
    Left ErrNotEnoughFunds{} ->
      throwIO (NotEnoughFuel :: PostTxError Tx)
    Left ErrScriptExecutionFailed{scriptFailure = (redeemerPtr, scriptFailure)} ->
      throwIO
        ( ScriptFailedInWallet
            { redeemerPtr = show redeemerPtr
            , failureReason = show scriptFailure
            } ::
            PostTxError Tx
        )
    Left e ->
      throwIO
        ( InternalWalletError
            { headUTxO
            , reason = show e
            , tx = fromLedgerTx partialTx
            } ::
            PostTxError Tx
        )
    Right validatedTx -> do
      pure $ sign validatedTx

-- * Following the Chain

-- | A /handler/ that takes care of following the chain.
data ChainSyncHandler m = ChainSyncHandler
  { onRollForward :: Block -> m ()
  , onRollBackward :: Point Block -> m ()
  }

-- | Conversion of a slot number to a time failed. This can be usually be
-- considered an internal error and may be happening because the used era
-- history is too old.
data TimeConversionException = TimeConversionException
  { slotNo :: SlotNo
  , reason :: Text
  }
  deriving (Eq, Show, Exception)

-- | Creates a `ChainSyncHandler` that can notify the given `callback` of events happening
-- on-chain.
--
-- This forms the other half of a `ChainComponent` along with `mkChain` but is decoupled from
-- actual interactions with the chain.
--
-- A `TimeHandle` is needed to do `SlotNo -> POSIXTime` conversions for 'Tick' events.
--
-- Throws 'TimeConversionException' when a received block's 'SlotNo' cannot be
-- converted to a 'UTCTime' with the given 'TimeHandle'.
chainSyncHandler ::
  forall m.
  (MonadSTM m, MonadThrow m) =>
  -- | Tracer for logging
  Tracer m DirectChainLog ->
  ChainCallback Tx m ->
  -- | Means to acquire a new 'TimeHandle'.
  GetTimeHandle m ->
  -- | Contextual information about our chain connection.
  ChainContext ->
  -- | A chain-sync handler to use in a local-chain-sync client.
  ChainSyncHandler m
chainSyncHandler tracer callback getTimeHandle ctx =
  ChainSyncHandler
    { onRollBackward
    , onRollForward
    }
 where
  onRollBackward :: Point Block -> m ()
  onRollBackward rollbackPoint = do
    let point = fromConsensusPointInMode CardanoMode rollbackPoint
    traceWith tracer $ RolledBackward{point}
    callback (const . Just $ Rollback $ chainSlotFromPoint point)

  onRollForward :: Block -> m ()
  onRollForward blk = do
    let point = fromConsensusPointInMode CardanoMode $ blockPoint blk
    let receivedTxs = map fromLedgerTx . toList $ getBabbageTxs blk
    traceWith tracer $
      RolledForward
        { point
        , receivedTxIds = getTxId . getTxBody <$> receivedTxs
        }

    case chainPointToSlotNo point of
      Nothing -> pure ()
      Just slotNo -> do
        timeHandle <- getTimeHandle
        case slotToUTCTime timeHandle slotNo of
          Left reason ->
            throwIO TimeConversionException{slotNo, reason}
          Right utcTime ->
            callback (const . Just $ Tick utcTime)

    forM_ receivedTxs $ \tx ->
      callback $ \ChainStateAt{chainState = cs} ->
        case observeSomeTx ctx cs tx of
          Nothing -> Nothing
          Just (observedTx, cs') ->
            Just $
              Observation
                { observedTx
                , newChainState =
                    ChainStateAt
                      { chainState = cs'
                      , recordedAt = Just point
                      }
                }

prepareTxToPost ::
  (MonadSTM m, MonadThrow (STM m)) =>
  TimeHandle ->
  TinyWallet m ->
  ChainContext ->
  ChainStateType Tx ->
  PostChainTx Tx ->
  STM m Tx
prepareTxToPost timeHandle wallet ctx cst@ChainStateAt{chainState} tx =
  case (tx, chainState) of
    (InitTx params, Idle) ->
      getSeedInput wallet >>= \case
        Just seedInput ->
          pure $ initialize ctx params seedInput
        Nothing ->
          throwIO (NoSeedInput @Tx)
    (AbortTx{utxo}, Initial st) ->
      pure $ abort ctx st utxo
    -- NOTE / TODO: 'CommitTx' also contains a 'Party' which seems redundant
    -- here. The 'Party' is already part of the state and it is the only party
    -- which can commit from this Hydra node.
    (CommitTx{committed}, Initial st) ->
      either throwIO pure (commit ctx st committed)
    -- TODO: We do not rely on the utxo from the collect com tx here because the
    -- chain head-state is already tracking UTXO entries locked by commit scripts,
    -- and thus, can re-construct the committed UTXO for the collectComTx from
    -- the commits' datums.
    --
    -- Perhaps we do want however to perform some kind of sanity check to ensure
    -- that both states are consistent.
    (CollectComTx{}, Initial st) ->
      pure $ collect ctx st
    (CloseTx{confirmedSnapshot}, Open st) -> do
      (currentSlot, currentTime) <- throwLeft currentPointInTime
      upperBound <- calculateTxUpperBoundFromContestationPeriod currentTime
      pure (close ctx st confirmedSnapshot currentSlot upperBound)
    (ContestTx{confirmedSnapshot}, Closed st) -> do
      (_, currentTime) <- throwLeft currentPointInTime
      upperBound <- calculateTxUpperBoundFromContestationPeriod currentTime
      pure (contest ctx st confirmedSnapshot upperBound)
    (FanoutTx{utxo, contestationDeadline}, Closed st) -> do
      deadlineSlot <- throwLeft $ slotFromUTCTime contestationDeadline
      pure (fanout st utxo deadlineSlot)
    (_, _) -> throwIO $ InvalidStateToPost{txTried = tx, chainState = cst}
 where
  -- XXX: Might want a dedicated exception type here
  throwLeft = either (throwSTM . userError . toString) pure

  TimeHandle{currentPointInTime, slotFromUTCTime} = timeHandle

  -- See ADR21 for context
  calculateTxUpperBoundFromContestationPeriod currentTime = do
    let effectiveDelay = min (toNominalDiffTime $ contestationPeriod ctx) maxGraceTime
    let upperBoundTime = addUTCTime effectiveDelay currentTime
    upperBoundSlot <- throwLeft $ slotFromUTCTime upperBoundTime
    pure (upperBoundSlot, upperBoundTime)

-- | Maximum delay we put on the upper bound of transactions to fit into a block.
-- NOTE: This is highly depending on the network. If the security parameter and
-- epoch length result in a short horizon, this is problematic.
maxGraceTime :: NominalDiffTime
maxGraceTime = 200

--
-- Helpers
--

-- | This extract __Babbage__ transactions from a block. If the block wasn't
-- produced in the Babbage era, it returns an empty sequence.
getBabbageTxs :: Block -> StrictSeq (ValidatedTx LedgerEra)
getBabbageTxs = \case
  BlockBabbage (ShelleyBlock (Ledger.Block _ txsSeq) _) ->
    fromTxSeq txsSeq
  _ ->
    mempty

--
-- Tracing
--

data DirectChainLog
  = ToPost {toPost :: PostChainTx Tx}
  | PostingTx {txId :: Ledger.TxId StandardCrypto}
  | PostedTx {txId :: Ledger.TxId StandardCrypto}
  | PostingFailed {tx :: ValidatedTx LedgerEra, postTxError :: PostTxError Tx}
  | RolledForward {point :: ChainPoint, receivedTxIds :: [TxId]}
  | RolledBackward {point :: ChainPoint}
  | Wallet TinyWalletLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary DirectChainLog where
  arbitrary = genericArbitrary
