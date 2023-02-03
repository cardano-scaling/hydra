{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.CollectCom where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Gen (genForParty, genHash, genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeHeadOutputDatum,
  changeMintedTokens,
 )
import Hydra.Chain.Direct.Fixture (
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.Tx (
  InitialThreadOutput (..),
  assetNameFromVerificationKey,
  collectComTx,
  headValue,
  mkCommitDatum,
  mkHeadId,
  mkHeadOutput,
  mkInitialOutput,
 )
import Hydra.Chain.Direct.Util (addChangeOutput)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (genAdaOnlyUTxO, genTxIn, genVerificationKey)
import Hydra.Party (Party, partyToChain)
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck (elements, oneof, suchThat)
import Test.QuickCheck.Instances ()
import qualified Prelude

--
-- CollectComTx
--

healthyCollectComTx :: (Tx, UTxO)
healthyCollectComTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (healthyHeadInput, healthyHeadResolvedInput) <> UTxO (txOut <$> healthyCommits)

  tx =
    addChangeOutput $
      collectComTx
        testNetworkId
        somePartyCardanoVerificationKey
        initialThreadOutput
        ((txOut &&& scriptData) <$> healthyCommits)
        (mkHeadId testPolicyId)

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  headDatum = fromPlutusData $ toData healthyCollectComInitialDatum

  initialThreadOutput =
    InitialThreadOutput
      { initialThreadUTxO = (healthyHeadInput, healthyHeadResolvedInput, headDatum)
      , initialParties = healthyOnChainParties
      , initialContestationPeriod = healthyContestationPeriod
      }

healthyCommits :: Map TxIn HealthyCommit
healthyCommits =
  (uncurry healthyCommitOutput <$> zip healthyParties committedUTxO)
    & Map.fromList
 where
  committedUTxO =
    generateWith
      (replicateM (length healthyParties) genCommittableTxOut)
      42

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyHeadInput :: TxIn
healthyHeadInput =
  generateWith arbitrary 42

healthyHeadResolvedInput :: TxOut CtxUTxO
healthyHeadResolvedInput =
  mkHeadOutput
    testNetworkId
    testPolicyId
    (toUTxOContext $ mkTxOutDatum healthyCollectComInitialDatum)

healthyCollectComInitialDatum :: Head.State
healthyCollectComInitialDatum =
  Head.Initial
    { contestationPeriod = healthyContestationPeriod
    , parties = healthyOnChainParties
    , headId = toPlutusCurrencySymbol testPolicyId
    }

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties =
  partyToChain <$> healthyParties

healthyParties :: [Party]
healthyParties = flip generateWith 42 $ do
  alice <- arbitrary
  bob <- arbitrary
  carol <- arbitrary
  pure [alice, bob, carol]

genCommittableTxOut :: Gen (TxIn, TxOut CtxUTxO)
genCommittableTxOut =
  Prelude.head . UTxO.pairs <$> (genAdaOnlyUTxO `suchThat` (\u -> length u > 1))

data HealthyCommit = HealthyCommit
  { cardanoKey :: VerificationKey PaymentKey
  , txOut :: TxOut CtxUTxO
  , scriptData :: ScriptData
  }
  deriving (Show)

healthyCommitOutput ::
  Party ->
  (TxIn, TxOut CtxUTxO) ->
  (TxIn, HealthyCommit)
healthyCommitOutput party committed =
  ( txIn
  , HealthyCommit
      { cardanoKey
      , txOut = toCtxUTxOTxOut (TxOut commitAddress commitValue (mkTxOutDatum commitDatum) ReferenceScriptNone)
      , scriptData = fromPlutusData (toData commitDatum)
      }
  )
 where
  txIn = genTxIn `genForParty` party

  cardanoKey = genVerificationKey `genForParty` party

  commitScript =
    fromPlutusScript Commit.validatorScript
  commitAddress =
    mkScriptAddress @PlutusScriptV2 testNetworkId commitScript
  commitValue =
    headValue
      <> (txOutValue . snd) committed
      <> valueFromList
        [ (AssetId testPolicyId (assetNameFromVerificationKey cardanoKey), 1)
        ]
  commitDatum =
    mkCommitDatum party Head.validatorHash (Just committed) (toPlutusCurrencySymbol $ headPolicyId healthyHeadInput)

data CollectComMutation
  = MutateOpenUTxOHash
  | -- | Test that collectCom cannot collect from an initial UTxO.
    MutateCommitToInitial
  | MutateHeadTransition
  | -- | Test that every party has a chance to commit.
    MutateNumberOfParties
  | MutateHeadId
  | MutateRequiredSigner
  | -- | Minting or burning of the tokens should not be possible in v_head apart from 'checkAbort' or 'checkFanout'
    MutateTokenMintingOrBurning
  deriving (Generic, Show, Enum, Bounded)

genCollectComMutation :: (Tx, UTxO) -> Gen SomeMutation
genCollectComMutation (tx, _utxo) =
  oneof
    [ SomeMutation Nothing MutateOpenUTxOHash . ChangeOutput 0 <$> mutateUTxOHash
    , SomeMutation Nothing MutateHeadTransition <$> do
        changeRedeemer <- ChangeHeadRedeemer <$> (Head.Close <$> arbitrary)
        differencCurrencySymbol <- arbitrary `suchThat` (/= toPlutusCurrencySymbol testPolicyId)
        changeDatum <-
          ChangeInputHeadDatum
            <$> ( Head.Open
                    <$> arbitrary
                    <*> arbitrary
                    <*> (toBuiltin <$> genHash)
                    <*> pure differencCurrencySymbol
                )
        pure $ Changes [changeRedeemer, changeDatum]
    , SomeMutation Nothing MutateNumberOfParties <$> do
        -- NOTE: This also mutates the contestation period becuase we could not
        -- be bothered to decode/lookup the current one.
        c <- arbitrary
        moreParties <- (: healthyOnChainParties) <$> arbitrary
        pure $
          Changes
            [ ChangeInputHeadDatum $ Head.Initial c moreParties (toPlutusCurrencySymbol testPolicyId)
            , ChangeOutput 0 $ mutatedPartiesHeadTxOut moreParties headTxOut
            ]
    , SomeMutation Nothing MutateHeadId <$> do
        illedHeadResolvedInput <-
          mkHeadOutput
            <$> pure testNetworkId
            <*> fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
            <*> pure (toUTxOContext $ mkTxOutDatum healthyCollectComInitialDatum)
        return $ ChangeInput healthyHeadInput illedHeadResolvedInput (Just $ toScriptData Head.CollectCom)
    , SomeMutation Nothing MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation Nothing MutateCommitToInitial <$> do
        (txIn, HealthyCommit{cardanoKey}) <- elements $ Map.toList healthyCommits
        pure $ ChangeInput txIn (toUTxOContext $ mkInitialOutput testNetworkId testPolicyId cardanoKey) Nothing
    , SomeMutation (Just "minting or burning is forbidden") MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  mutatedPartiesHeadTxOut parties =
    changeHeadOutputDatum $ \case
      Head.Open{utxoHash, contestationPeriod, headId} ->
        Head.Open{Head.parties = parties, contestationPeriod, utxoHash, headId}
      st -> error $ "Unexpected state " <> show st

  mutateUTxOHash = do
    mutatedUTxOHash <- genHash
    pure $ changeHeadOutputDatum (mutateState mutatedUTxOHash) headTxOut

  mutateState mutatedUTxOHash = \case
    Head.Open{parties, contestationPeriod, headId} ->
      Head.Open{parties, contestationPeriod, Head.utxoHash = toBuiltin mutatedUTxOHash, headId}
    st -> st
