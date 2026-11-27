-- | Specifies the /Head-Chain Interaction/ part of the protocol
--
-- Incoming and outgoing on-chain transactions are modelled respectively as `OnChainTx`
-- and `PostChainTx` which are data type that abstracts away the details of the structure
-- of the transaction.
module Hydra.Chain (module Hydra.Chain, module Hydra.Chain.Types) where

import Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import Hydra.Cardano.Api (AddressInEra, LedgerEra, Value)
import Hydra.Chain.Types
import Hydra.Tx (CommitBlueprintTx, ConfirmedSnapshot, HeadId)

-- | Handle to interface with the main chain network
data Chain tx m = Chain
  { postTx :: MonadThrow m => PostChainTx tx -> m ()
  -- ^ Construct and send a transaction to the main chain corresponding to the
  -- given 'PostChainTx' description.
  -- This function is not expected to block, so it is only responsible for
  -- submitting, but it should validate the created transaction against a
  -- reasonable local view of the chain and throw an exception when invalid.
  --
  -- Does at least throw 'PostTxError'.
  , draftDepositTx ::
      MonadThrow m =>
      HeadId ->
      PParams LedgerEra ->
      ConfirmedSnapshot tx ->
      CommitBlueprintTx tx ->
      UTCTime ->
      Maybe AddressInEra ->
      m (Either (PostTxError tx) tx)
  -- ^ Create a deposit transaction using user provided utxos (zero or many) ,
  -- _blueprint_ transaction which spends these outputs and a deadline for
  -- their inclusion into L2. The current confirmed snapshot serves as the
  -- basis for a dry-run increment transaction rejecting deposits which could
  -- never be claimed. Errors are handled at the call site.
  , submitTx :: MonadThrow m => tx -> m ()
  -- ^ Submit a cardano transaction.
  --
  -- Throws at least 'PostTxError'.
  --
  -- XXX: While technically they could be any of 'PostTxError tx', only
  -- `FailedToPostTx` errors are expected here.
  , checkNonADAAssets :: ConfirmedSnapshot tx -> Either Value ()
  }

-- | A callback indicating a 'ChainEvent tx' happened. Most importantly the
-- 'Observation' of a relevant Hydra transaction.
type ChainCallback tx m = ChainEvent tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
