{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.CollectCom where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-plutus" Hydra.Plutus.Gen ()
import "hydra-prelude" Hydra.Prelude hiding (label, toList)
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (choose, elements, oneof, suchThat)
import "base" Data.List qualified as List
import "base" Data.Maybe (fromJust)
import "base" GHC.IsList (IsList (..))
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "containers" Data.Map qualified as Map
import "hydra-cardano-api" Hydra.Cardano.Api.Gen (genTxIn)
import "hydra-plutus" Hydra.Contract.CommitError (CommitError (STIsMissingInTheOutput))
import "hydra-plutus" Hydra.Contract.Error (toErrorCode)
import "hydra-plutus" Hydra.Contract.HeadError (HeadError (..))
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Contract.HeadTokens (headPolicyId)
import "hydra-plutus" Hydra.Contract.Initial qualified as Initial
import "hydra-plutus" Hydra.Contract.InitialError (InitialError (ExpectedSingleCommitOutput, LockedValueDoesNotMatch))
import "hydra-plutus" Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import "hydra-plutus" Hydra.Data.Party qualified as OnChain
import "hydra-plutus" Hydra.Plutus (commitValidatorScript, initialValidatorScript)

import Hydra.Tx (HeadParameters (..), Party, partyToChain)
import Hydra.Tx.CollectCom (
  collectComTx,
 )
import Hydra.Tx.Commit (mkCommitDatum)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.ContestationPeriod qualified as ContestationPeriod
import Hydra.Tx.Contract.Commit (genMintedOrBurnedValue)
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.Init (mkHeadOutput, mkInitialOutput)
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Utils (
  hydraHeadV1AssetName,
  onChainIdToAssetName,
  verificationKeyToOnChainId,
 )
import Test.Hydra.Tx.Fixture (
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genForParty,
  genHash,
  genScriptRegistry,
  genUTxOAdaOnlyOfSize,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceParties,
 )
import "plutus-tx" PlutusTx.Builtins (toBuiltin)
import "quickcheck-instances" Test.QuickCheck.Instances ()

--
-- CollectComTx
--

healthyCollectComTx :: (Tx, UTxO)
healthyCollectComTx =
  (tx, lookupUTxO)
 where
  commitOutputs = txOut <$> healthyCommits
  committedUTxO = foldMap committed $ Map.elems healthyCommits

  lookupUTxO =
    UTxO.singleton healthyHeadTxIn healthyHeadTxOut
      <> UTxO commitOutputs
      <> registryUTxO scriptRegistry

  tx =
    collectComTx
      testNetworkId
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId testPolicyId)
      parameters
      (healthyHeadTxIn, healthyHeadTxOut)
      commitOutputs
      committedUTxO

  scriptRegistry = genScriptRegistry `generateWith` 42

  somePartyCardanoVerificationKey = elements healthyParticipants `generateWith` 42

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyCommits :: Map TxIn HealthyCommit
healthyCommits =
  Map.fromList
    <$> flip generateWith 42
    $ mapM createHealthyCommit
    $ zip healthyParticipants healthyParties
 where
  createHealthyCommit (vk, party) = do
    utxo <- genUTxOAdaOnlyOfSize =<< choose (0, 5)
    pure $ healthyCommitOutput (verificationKeyToOnChainId vk) party utxo

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
    (mkTxOutDatumInline healthyCollectComInitialDatum)

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

data HealthyCommit = HealthyCommit
  { participant :: OnChainId
  , txOut :: TxOut CtxUTxO
  , committed :: UTxO
  }
  deriving stock (Show)

healthyCommitOutput ::
  OnChainId ->
  Party ->
  UTxO ->
  (TxIn, HealthyCommit)
healthyCommitOutput participant party committed =
  ( txIn
  , HealthyCommit
      { participant
      , txOut = toCtxUTxOTxOut (TxOut commitAddress commitValue (mkTxOutDatumInline commitDatum) ReferenceScriptNone)
      , committed
      }
  )
 where
  txIn = genTxIn `genForParty` party

  commitAddress =
    mkScriptAddress testNetworkId commitValidatorScript
  commitValue =
    UTxO.totalValue committed
      <> fromList
        [ (AssetId testPolicyId (onChainIdToAssetName participant), 1)
        ]
  commitDatum =
    mkCommitDatum party committed (toPlutusCurrencySymbol $ headPolicyId healthyHeadTxIn)

data CollectComMutation
  = -- | Ensures collectCom does not allow any output address but νHead.
    NotContinueContract
  | -- | Needs to prevent that not all value is collected into the head output.
    ExtractSomeValue
  | MutateOpenUTxOHash
  | MutateOpenVersion
  | -- | Ensures collectCom cannot collect from an initial UTxO.
    MutateCommitToInitial
  | -- | Every party should have committed and been taken into account for the
    -- collectCom transaction to be valid. Here we increase the number of
    -- parties in input and output but keep the commits unchanged. This
    -- simulates the situation where one participant would not have committed
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
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , SomeMutation (pure $ toErrorCode NotAllValueCollected) ExtractSomeValue <$> do
        extractHeadOutputValue headTxOut testPolicyId
    , SomeMutation (pure $ toErrorCode IncorrectUtxoHash) MutateOpenUTxOHash . ChangeOutput 0 <$> do
        mutatedUTxOHash <- genHash
        pure $ modifyInlineDatum (mutateState mutatedUTxOHash) headTxOut
    , SomeMutation (pure $ toErrorCode IncorrectVersion) MutateOpenVersion . ChangeOutput 0 <$> do
        mutatedVersion <- arbitrary `suchThat` (/= 0)
        pure $ modifyInlineDatum (mutateStateVersion mutatedVersion) headTxOut
    , SomeMutation (pure $ toErrorCode MissingCommits) MutateNumberOfParties <$> do
        moreParties <- (: healthyOnChainParties) <$> arbitrary
        pure $
          Changes
            [ ChangeInputHeadDatum $ replaceParties moreParties healthyCollectComInitialDatum
            , ChangeOutput 0 $ mutatedPartiesHeadTxOut moreParties headTxOut
            ]
    , SomeMutation (pure $ toErrorCode STNotSpent) MutateHeadId <$> do
        -- XXX: This mutation is unrealistic. It would only change the headId in
        -- the value, but not in the datum. This is not allowed by the protocol
        -- prior to this transaction.
        illedHeadResolvedInput <-
          mkHeadOutput testNetworkId
            <$> fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
            <*> pure (mkTxOutDatumInline healthyCollectComInitialDatum)
        return $ ChangeInput healthyHeadTxIn illedHeadResolvedInput (Just $ toScriptData Head.CollectCom)
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (map toErrorCode [ExpectedSingleCommitOutput, LockedValueDoesNotMatch]) MutateCommitToInitial <$> do
        -- By changing a commit output to an initial, we simulate a situation
        -- where we do pretend to have collected every commit, but we just
        -- changed one back to be an initial. This should be caught by the
        -- initial validator.
        (txIn, HealthyCommit{participant}) <- elements $ Map.toList healthyCommits
        pure $
          Changes
            [ ChangeInput
                txIn
                (toCtxUTxOTxOut $ mkInitialOutput testNetworkId testSeedInput participant)
                (Just . toScriptData . Initial.redeemer $ Initial.ViaCommit [toPlutusTxOutRef txIn])
            , AddScript initialValidatorScript
            ]
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (pure $ toErrorCode STIsMissingInTheOutput) RemoveSTFromOutput <$> do
        let out = List.head $ txOuts' tx
        let stAssetId = AssetId (headPolicyId testSeedInput) hydraHeadV1AssetName
        let newValue = filterValue (/= stAssetId) (txOutValue out)
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  mutatedPartiesHeadTxOut parties =
    modifyInlineDatum $ \case
      Head.Open Head.OpenDatum{contestationPeriod, headId, version, utxoHash} ->
        Head.Open Head.OpenDatum{parties, contestationPeriod, headId, version, utxoHash}
      st -> error $ "Unexpected state " <> show st

  mutateState :: ByteString -> Head.State -> Head.State
  mutateState mutatedUTxOHash = \case
    Head.Open Head.OpenDatum{parties, contestationPeriod, headId, version} ->
      Head.Open Head.OpenDatum{Head.utxoHash = toBuiltin mutatedUTxOHash, parties, contestationPeriod, headId, version}
    st -> st

  mutateStateVersion mutatedVersion = \case
    Head.Open Head.OpenDatum{parties, contestationPeriod, headId, utxoHash} ->
      Head.Open Head.OpenDatum{version = mutatedVersion, parties, contestationPeriod, headId, utxoHash}
    st -> st

-- | Remove a random asset and quantity from headOutput by adding another output
-- that "extracts" that value.
extractHeadOutputValue :: TxOut CtxTx -> PolicyId -> Gen Mutation
extractHeadOutputValue headTxOut policyId = do
  removedValue <- do
    let allAssets = toList $ txOutValue headTxOut
        nonPTs = flip filter allAssets $ \case
          (AssetId pid _, _) -> pid /= policyId
          _ -> True
    (assetId, Quantity n) <- elements nonPTs
    q <- Quantity <$> choose (1, n)
    pure $ fromList [(assetId, q)]
  -- Add another output which would extract the 'removedValue'. The ledger would
  -- require this to have a balanced transaction.
  extractionTxOut <- do
    someAddress <- genAddressInEra testNetworkId
    pure $ TxOut someAddress removedValue TxOutDatumNone ReferenceScriptNone
  pure $
    Changes
      [ ChangeOutput 0 $ modifyTxOutValue (\v -> v <> negateValue removedValue) headTxOut
      , AppendOutput extractionTxOut
      ]
