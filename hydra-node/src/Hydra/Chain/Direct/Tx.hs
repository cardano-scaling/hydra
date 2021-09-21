{-# LANGUAGE TypeApplications #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Crypto.Hash (hashFromBytes)
import Cardano.Ledger.Address (Addr (Addr))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (PlutusScript), Tag (Spend))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (RdmrPtr), Redeemers (..), TxDats (..), TxWitness (..), unTxDats)
import Cardano.Ledger.Crypto (ADDRHASH, StandardCrypto)
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
import Plutus.V1.Ledger.Api (ValidatorHash (ValidatorHash), fromBuiltin, fromData, toBuiltinData, toData)
import Shelley.Spec.Ledger.API (
  Coin (..),
  Credential (ScriptHashObj),
  Network (Testnet),
  ScriptHash (ScriptHash),
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxIn,
  Wdrl (Wdrl),
 )
import Shelley.Spec.Ledger.Tx (hashScript)

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
  AbortTx _utxo -> abortTx txIn
  _ -> error "not implemented"

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx :: HeadParameters -> TxIn StandardCrypto -> ValidatedTx Era
initTx HeadParameters{contestationPeriod, parties} txIn =
  mkUnsignedTx body dats mempty
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
  headAddress = validatorHashToAddr Initial.validatorHash

  -- REVIEW(SN): how much to store here / minUtxoValue / depending on assets?
  headValue = inject (Coin 0)

  headDatumHash = hashData @Era headDatum

  headDatum =
    Data . toData $
      Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

abortTx :: TxIn StandardCrypto -> ValidatedTx Era
abortTx txIn =
  mkUnsignedTx body mempty redeemers
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
  ValidatedTx Era
mkUnsignedTx body datums redeemers =
  ValidatedTx
    { body
    , wits =
        TxWitness
          { txwitsVKey = mempty
          , txwitsBoot = mempty
          , txscripts = mempty
          , txdats = datums
          , txrdmrs = Redeemers redeemers
          }
    , isValid = IsValid True -- REVIEW(SN): no idea of the semantics of this
    , auxiliaryData = SNothing
    }

-- | Convert a plutus address to its ledger representation.
validatorHashToAddr :: ValidatorHash -> Addr StandardCrypto
validatorHashToAddr (ValidatorHash builtinByteString) =
  Addr
    network
    (ScriptHashObj $ ScriptHash hash)
    -- REVIEW(SN): stake head funds?
    StakeRefNull
 where
  hash = case hashFromBytes @(ADDRHASH StandardCrypto) bytes of
    Nothing -> error $ "ValidatorHash is not (the right) hash: " <> show bytes
    Just h -> h

  bytes = fromBuiltin builtinByteString
