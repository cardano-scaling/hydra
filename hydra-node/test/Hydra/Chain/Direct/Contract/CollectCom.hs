{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.CollectCom where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Gen (genForParty, genHash, genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceParties,
 )
import Hydra.Chain.Direct.Fixture (
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  assetNameFromVerificationKey,
  collectComTx,
  hydraHeadV1AssetName,
  mkCommitDatum,
  mkHeadId,
  mkHeadOutput,
  mkInitialOutput,
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.ContestationPeriod qualified as ContestationPeriod
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.CommitError (CommitError (STIsMissingInTheOutput))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.InitialError (InitialError (ExpectedSingleCommitOutput))
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano (
  genAdaOnlyUTxO,
  genAddressInEra,
  genUTxOAdaOnlyOfSize,
  genVerificationKey,
 )
import Hydra.Party (Party, partyToChain)
import Hydra.Plutus.Orphans ()
import PlutusTx.Builtins (toBuiltin)
import Test.QuickCheck (choose, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()
import Prelude qualified

--
-- CollectComTx
--

healthyCollectComTx :: (Tx, UTxO)
healthyCollectComTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (healthyHeadTxIn, healthyHeadTxOut)
      <> UTxO (txOut <$> healthyCommits)
      <> registryUTxO scriptRegistry

  tx =
    collectComTx
      testNetworkId
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId testPolicyId)
      parameters
      (healthyHeadTxIn, healthyHeadTxOut)
      (txOut <$> healthyCommits)

  scriptRegistry = genScriptRegistry `generateWith` 42

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

healthyCommits :: Map TxIn HealthyCommit
healthyCommits =
  (uncurry healthyCommitOutput <$> zip healthyParties healthyCommittedUTxO)
    & Map.fromList

healthyCommittedUTxO :: [UTxO]
healthyCommittedUTxO =
  flip generateWith 42 $
    replicateM (length healthyParties) $
      genUTxOAdaOnlyOfSize =<< choose (0, 5)

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyHeadTxIn :: TxIn
healthyHeadTxIn =
  generateWith arbitrary 42

healthyHeadTxOut :: TxOut CtxUTxO
healthyHeadTxOut =
  mkHeadOutput
    testNetworkId
    testPolicyId
    (toUTxOContext $ mkTxOutDatumInline healthyCollectComInitialDatum)

healthyCollectComInitialDatum :: Head.State
healthyCollectComInitialDatum =
  Head.Initial
    { contestationPeriod = ContestationPeriod.toChain healthyContestationPeriod
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
  }
  deriving stock (Show)

healthyCommitOutput ::
  Party ->
  UTxO ->
  (TxIn, HealthyCommit)
healthyCommitOutput party committed =
  ( txIn
  , HealthyCommit
      { cardanoKey
      , txOut = toCtxUTxOTxOut (TxOut commitAddress commitValue (mkTxOutDatumInline commitDatum) ReferenceScriptNone)
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
    foldMap txOutValue committed
      <> valueFromList
        [ (AssetId testPolicyId (assetNameFromVerificationKey cardanoKey), 1)
        ]
  commitDatum =
    mkCommitDatum party committed (toPlutusCurrencySymbol $ headPolicyId healthyHeadTxIn)

data CollectComMutation
  = -- | Ensures collectCom does not allow any output address but νHead.
    NotContinueContract
  | -- | Needs to prevent that not all value is collected into the head output.
    ExtractSomeValue
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
  | -- | νCommit validator checks the ST is in the output
    RemoveSTFromOutput
  deriving stock (Generic, Show, Enum, Bounded)

genCollectComMutation :: (Tx, UTxO) -> Gen SomeMutation
genCollectComMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (Just $ toErrorCode NotAllValueCollected) ExtractSomeValue <$> do
        -- Remove a random asset and quantity from headOutput
        removedValue <- do
          let allAssets = valueToList $ txOutValue headTxOut
              nonPTs = flip filter allAssets $ \case
                (AssetId pid _, _) -> pid /= testPolicyId
                _ -> True
          (assetId, Quantity n) <- elements nonPTs
          q <- Quantity <$> choose (1, n)
          pure $ valueFromList [(assetId, q)]
        -- Add another output which would extract the 'removedValue'. The ledger
        -- would check for this, and this is needed because the way we implement
        -- collectCom checks.
        extractionTxOut <- do
          someAddress <- genAddressInEra testNetworkId
          pure $ TxOut someAddress removedValue TxOutDatumNone ReferenceScriptNone
        pure $
          Changes
            [ ChangeOutput 0 $ modifyTxOutValue (\v -> v <> negateValue removedValue) headTxOut
            , AppendOutput extractionTxOut
            ]
    , SomeMutation (Just $ toErrorCode IncorrectUtxoHash) MutateOpenUTxOHash . ChangeOutput 0 <$> mutateUTxOHash
    , SomeMutation (Just $ toErrorCode MissingCommits) MutateNumberOfParties <$> do
        moreParties <- (: healthyOnChainParties) <$> arbitrary
        pure $
          Changes
            [ ChangeInputHeadDatum $ replaceParties moreParties healthyCollectComInitialDatum
            , ChangeOutput 0 $ mutatedPartiesHeadTxOut moreParties headTxOut
            ]
    , SomeMutation (Just $ toErrorCode STNotSpent) MutateHeadId <$> do
        -- XXX: This mutation is unrealistic. It would only change the headId in
        -- the value, but not in the datum. This is not allowed by the protocol
        -- prior to this transaction.
        illedHeadResolvedInput <-
          mkHeadOutput
            <$> pure testNetworkId
            <*> fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
            <*> pure (toUTxOContext $ mkTxOutDatumInline healthyCollectComInitialDatum)
        return $ ChangeInput healthyHeadTxIn illedHeadResolvedInput (Just $ toScriptData Head.CollectCom)
    , SomeMutation (Just $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (Just $ toErrorCode ExpectedSingleCommitOutput) MutateCommitToInitial <$> do
        -- By changing a commit output to an initial, we simulate a situation
        -- where we do pretend to have collected every commit, but we just
        -- changed one back to be an initial. This should be caught by the
        -- initial validator.
        (txIn, HealthyCommit{cardanoKey}) <- elements $ Map.toList healthyCommits
        pure $
          Changes
            [ ChangeInput
                txIn
                (toUTxOContext $ mkInitialOutput testNetworkId testSeedInput cardanoKey)
                (Just . toScriptData . Initial.redeemer $ Initial.ViaCommit [toPlutusTxOutRef txIn])
            , AddScript $ fromPlutusScript Initial.validatorScript
            ]
    , SomeMutation (Just $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (Just $ toErrorCode STIsMissingInTheOutput) RemoveSTFromOutput <$> do
        let out = List.head $ txOuts' tx
        let stAssetId = AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName
        let newValue = filterValue (/= stAssetId) (txOutValue out)
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  mutatedPartiesHeadTxOut parties =
    modifyInlineDatum $ \case
      Head.Open{utxoHash, contestationPeriod, headId} ->
        Head.Open{Head.parties = parties, contestationPeriod, utxoHash, headId}
      st -> error $ "Unexpected state " <> show st

  mutateUTxOHash = do
    mutatedUTxOHash <- genHash
    pure $ modifyInlineDatum (mutateState mutatedUTxOHash) headTxOut

  mutateState mutatedUTxOHash = \case
    Head.Open{parties, contestationPeriod, headId} ->
      Head.Open{parties, contestationPeriod, Head.utxoHash = toBuiltin mutatedUTxOHash, headId}
    st -> st
