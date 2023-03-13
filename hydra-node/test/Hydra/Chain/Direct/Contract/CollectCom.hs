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
  replaceParties,
 )
import Hydra.Chain.Direct.Fixture (
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
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
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (genAdaOnlyUTxO, genAddressInEra, genTxIn, genVerificationKey)
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
    UTxO.singleton (healthyHeadInput, healthyHeadResolvedInput)
      <> UTxO (txOut <$> healthyCommits)
      <> registryUTxO scriptRegistry

  tx =
    collectComTx
      testNetworkId
      scriptRegistry
      somePartyCardanoVerificationKey
      initialThreadOutput
      ((txOut &&& scriptData) <$> healthyCommits)
      (mkHeadId testPolicyId)

  scriptRegistry = genScriptRegistry `generateWith` 42

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
    , seed = toPlutusTxOutRef testSeedInput
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
    mkCommitDatum party (Just committed) (toPlutusCurrencySymbol $ headPolicyId healthyHeadInput)

data CollectComMutation
  = -- | Ensures collectCom does not allow any output address but νHead.
    NotContinueContract
  | MutateOpenUTxOHash
  | -- | Ensures collectCom cannot collect from an initial UTxO.
    MutateCommitToInitial
  | -- | Every party should have commited and been taken into account for the
    -- collectCom transaction to be valid. Here we increase the number of
    -- parties in input and output but keep the commits unchanged. This
    -- simulates the situation where one participant would not have commited
    -- already or whose commit would have been ignored by the collectCom
    -- transaction.
    MutateNumberOfParties
  | MutateHeadId
  | MutateRequiredSigner
  | -- | Minting or burning of tokens should not be possible in collectCom.
    MutateTokenMintingOrBurning
  deriving (Generic, Show, Enum, Bounded)

genCollectComMutation :: (Tx, UTxO) -> Gen SomeMutation
genCollectComMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (Just $ toErrorCode IncorrectUtxoHash) MutateOpenUTxOHash . ChangeOutput 0 <$> mutateUTxOHash
    , SomeMutation (Just $ toErrorCode MissingCommits) MutateNumberOfParties <$> do
        moreParties <- (: healthyOnChainParties) <$> arbitrary
        pure $
          Changes
            [ ChangeInputHeadDatum $ replaceParties moreParties healthyCollectComInitialDatum
            , ChangeOutput 0 $ mutatedPartiesHeadTxOut moreParties headTxOut
            ]
    , SomeMutation (Just $ toErrorCode STNotSpent) MutateHeadId <$> do
        illedHeadResolvedInput <-
          mkHeadOutput
            <$> pure testNetworkId
            <*> fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
            <*> pure (toUTxOContext $ mkTxOutDatum healthyCollectComInitialDatum)
        return $ ChangeInput healthyHeadInput illedHeadResolvedInput (Just $ toScriptData Head.CollectCom)
    , SomeMutation (Just $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (Just $ toErrorCode DatumNotFound) MutateCommitToInitial <$> do
        -- we're satisfied with "datum not found" as the current version of the validator will consider
        -- the initial input as if it were a commit input, hence fetching the datum which is expected
        -- in a commit and complaining that it did not find it
        (txIn, HealthyCommit{cardanoKey}) <- elements $ Map.toList healthyCommits
        pure $ ChangeInput txIn (toUTxOContext $ mkInitialOutput testNetworkId testSeedInput cardanoKey) Nothing
    , SomeMutation (Just $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
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
