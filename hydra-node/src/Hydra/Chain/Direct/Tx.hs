{-# LANGUAGE TypeApplications #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Address (Addr (Addr))
import Cardano.Ledger.Alonzo (AlonzoEra, Script)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (PlutusScript), Tag (Spend))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (RdmrPtr), Redeemers (..), TxDats (..), TxWitness (..), unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (hashScript)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (inject)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (..), OnChainTx (OnInitTx), PostChainTx (InitTx))
import Hydra.Contract.Head (State (Initial))
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import Hydra.Party (anonymousParty, vkey)
import Plutus.V1.Ledger.Api (PubKeyHash (..), fromData, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Shelley.Spec.Ledger.API (
  Coin (..),
  Credential (ScriptHashObj),
  Network (Testnet),
  ScriptHash,
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxIn,
  Wdrl (Wdrl),
 )

-- TODO(SN): parameterize
network :: Network
network = Testnet

type Era = AlonzoEra StandardCrypto

-- * Post Hydra Head transactions

-- | Construct the Head protocol transactions as Alonzo 'Tx'. Note that
-- 'ValidatedTx' this produces an unbalanced, unsigned transaction and this type
-- was used (in contrast to 'TxBody') to be able to express included datums,
-- onto which at least the 'initTx' relies on.
constructTx :: TxIn StandardCrypto -> PostChainTx tx -> ValidatedTx Era
constructTx txIn = \case
  InitTx p -> initTx p txIn
  AbortTx _utxo -> abortTx (txIn, error "where is this coming from?")
  _ -> error "not implemented"

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx :: HeadParameters -> TxIn StandardCrypto -> ValidatedTx Era
initTx HeadParameters{contestationPeriod, parties} txIn =
  mkUnsignedTx body dats mempty mempty
 where
  body =
    TxBody
      { inputs = Set.singleton txIn
      , collateral = mempty
      , -- TODO(SN): of course this is missing the PT outputs
        outputs = StrictSeq.singleton headOut
      , txcerts = mempty
      , txwdrls = Wdrl mempty
      , txfee = Coin 0
      , txvldt = ValidityInterval SNothing SNothing
      , txUpdates = SNothing
      , reqSignerHashes = mempty
      , mint = mempty
      , scriptIntegrityHash = SNothing
      , adHash = SNothing
      , txnetworkid = SNothing
      }

  dats = TxDats $ Map.singleton headDatumHash headDatum

  headOut = TxOut headAddress headValue (SJust headDatumHash)

  -- TODO(SN): The main Hydra Head script address. Will be parameterized by the
  -- thread token eventually. For now, this is just the initial script as well,
  -- although this could be really some arbitrary address. After all it is also
  -- later quite arbitrary/different per Head.
  headAddress = scriptAddr $ PlutusScript "foo"

  -- REVIEW(SN): how much to store here / minUtxoValue / depending on assets?
  headValue = inject (Coin 0)

  headDatumHash = hashData @Era headDatum

  headDatum =
    Data . toData $
      Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

-- | Create transaction which aborts by spending one input. This is currently
-- only possible if this is governed by the initial script and only for a single
-- input. Of course, the Head protocol specifies we need to spend ALL the Utxo
-- containing PTs.
abortTx :: (TxIn StandardCrypto, PubKeyHash) -> ValidatedTx Era
abortTx (txIn, pkh) =
  mkUnsignedTx body dats redeemers scripts
 where
  body =
    TxBody
      { inputs = Set.singleton txIn
      , collateral = mempty
      , outputs = mempty
      , txcerts = mempty
      , txwdrls = Wdrl mempty
      , txfee = Coin 0
      , txvldt = ValidityInterval SNothing SNothing
      , txUpdates = SNothing
      , reqSignerHashes = mempty
      , mint = mempty
      , scriptIntegrityHash = SNothing
      , adHash = SNothing
      , txnetworkid = SNothing
      }

  -- TODO(SN): dummy exUnits, balancing overrides them?
  redeemers = Map.singleton (RdmrPtr Spend 0) (redeemerData, ExUnits 0 0)

  -- TODO(SN): This should be 'Abort' or so
  redeemerData = Data $ toData ()

  scripts = Map.singleton initialScriptHash initialScript

  initialScriptHash = hashScript @Era initialScript

  initialScript = plutusScript Initial.validatorScript

  dats = TxDats $ Map.singleton initialDatumHash initialDatum

  initialDatumHash = hashData @Era initialDatum

  initialDatum = Data $ toData pkh

--

-- * Observe Hydra Head transactions

observeTx :: ValidatedTx Era -> Maybe (OnChainTx tx)
observeTx tx =
  observeInitTx tx
    <|> observeAbortTx tx

observeInitTx :: ValidatedTx Era -> Maybe (OnChainTx tx)
observeInitTx ValidatedTx{wits} = do
  (Data d) <- firstDatum
  fromData d >>= \case
    Initial cp ps ->
      pure $ OnInitTx (contestationPeriodToDiffTime cp) (map convertParty ps)
    _ -> Nothing
 where
  firstDatum = snd . head <$> nonEmpty datums

  datums = Map.toList . unTxDats $ txdats wits

  convertParty = anonymousParty . partyToVerKey

observeAbortTx :: ValidatedTx Era -> Maybe (OnChainTx tx)
observeAbortTx _ = Just OnAbortTx
--

-- * Helpers

mkUnsignedTx ::
  TxBody Era ->
  TxDats Era ->
  Map RdmrPtr (Data Era, ExUnits) ->
  Map (ScriptHash StandardCrypto) (Script Era) ->
  ValidatedTx Era
mkUnsignedTx body datums redeemers scripts =
  ValidatedTx
    { body
    , wits =
        TxWitness
          { txwitsVKey = mempty
          , txwitsBoot = mempty
          , txscripts = scripts
          , txdats = datums
          , txrdmrs = Redeemers redeemers
          }
    , isValid = IsValid True -- REVIEW(SN): no idea of the semantics of this
    , auxiliaryData = SNothing
    }

-- | Get the ledger address for a given plutus script.
scriptAddr :: Script Era -> Addr StandardCrypto
scriptAddr script =
  Addr
    network
    (ScriptHashObj $ hashScript @Era script)
    -- REVIEW(SN): stake head funds?
    StakeRefNull

plutusScript :: Plutus.Script -> Script Era
plutusScript = PlutusScript . toShort . fromLazy . serialize
