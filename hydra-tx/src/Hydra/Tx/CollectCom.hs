{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.CollectCom where

import Data.Map qualified as Map
import Hydra.Cardano.Api
import Hydra.Prelude

import Data.ByteString qualified as BS
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import Hydra.Plutus (commitValidatorScript)
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, mkHydraHeadV1TxName)
import PlutusLedgerApi.Common (fromBuiltin)
import PlutusLedgerApi.V3 (toBuiltin)
import Test.QuickCheck (vectorOf)

-- * Construction

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Parameters of the head to collect .
  HeadParameters ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  Tx
collectComTx networkId scriptRegistry vk headId headParameters (headInput, initialHeadOutput) commits utxoToCollect =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns ((headInput, headWitness) : (mkCommit <$> Map.keys commits))
      & addTxInsReference [commitScriptRef, headScriptRef]
      & addTxOuts [headOutput]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CollectComTx")
 where
  HeadParameters{parties, contestationPeriod} = headParameters

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer
  headScriptRef = fst (headReference scriptRegistry)
  headRedeemer = toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress networkId Head.validatorScript)
      (txOutValue initialHeadOutput <> commitValue)
      headDatumAfter
      ReferenceScriptNone
  headDatumAfter =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { Head.parties = partyToChain <$> parties
          , utxoHash
          , contestationPeriod = toChain contestationPeriod
          , headId = headIdToCurrencySymbol headId
          , version = 0
          }

  utxoHash = toBuiltin $ hashUTxO @Tx utxoToCollect

  mkCommit commitTxIn = (commitTxIn, commitWitness)
  commitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitValidatorScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitValue =
    mconcat $ txOutValue <$> Map.elems commits
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.ViaCollectCom

-- * Observation

-- | Representation of the Head output after a CollectCom transaction.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)

newtype UTxOHash = UTxOHash ByteString
  deriving stock (Eq, Show, Generic)

instance Arbitrary UTxOHash where
  arbitrary = UTxOHash . BS.pack <$> vectorOf 32 arbitrary

data CollectComObservation = CollectComObservation
  { threadOutput :: OpenThreadOutput
  , headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving stock (Show, Eq, Generic)

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CollectComObservation
observeCollectComTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Initial{parties, contestationPeriod}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
      pure
        CollectComObservation
          { threadOutput =
              OpenThreadOutput
                { openThreadUTxO = (newHeadInput, newHeadOutput)
                , openParties = parties
                , openContestationPeriod = contestationPeriod
                }
          , headId
          , utxoHash
          }
    _ -> Nothing
 where
  decodeUtxoHash datum =
    case fromScriptData datum of
      Just (Head.Open Head.OpenDatum{utxoHash}) -> Just $ fromBuiltin utxoHash
      _ -> Nothing
