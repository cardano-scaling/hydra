{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.CollectCom where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  anyPayToPubKeyTxOut,
  genHash,
  headTxIn,
 )
import Hydra.Chain.Direct.Fixture (
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.Tx (
  assetNameFromVerificationKey,
  collectComTx,
  convertPartyToChain,
  headPolicyId,
  headValue,
  mkCommitDatum,
  mkHeadOutput,
 )
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (genAdaOnlyUTxO, genKeyPair, genTxIn, genVerificationKey)
import Hydra.Party (Party, genForParty)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromData, toBuiltin, toData)
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
    UTxO.singleton (healthyHeadInput, healthyHeadResolvedInput) <> UTxO (fst <$> commits)

  tx =
    collectComTx
      testNetworkId
      somePartyCardanoVerificationKey
      (healthyHeadInput, healthyHeadResolvedInput, headDatum, healthyOnChainParties)
      commits

  somePartyCardanoVerificationKey = flip generateWith 42 $ do
    genForParty genVerificationKey <$> elements healthyParties

  committedUTxO =
    generateWith
      (replicateM (length healthyParties) genCommittableTxOut)
      42

  commits =
    (uncurry healthyCommitOutput <$> zip healthyParties committedUTxO)
      & Map.fromList

  headDatum = fromPlutusData $ toData healthyCollectComInitialDatum

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
    { contestationPeriod = generateWith arbitrary 42
    , parties = healthyOnChainParties
    }

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties =
  convertPartyToChain <$> healthyParties

healthyParties :: [Party]
healthyParties = flip generateWith 42 $ do
  alice <- arbitrary
  bob <- arbitrary
  carol <- arbitrary
  pure [alice, bob, carol]

genCommittableTxOut :: Gen (TxIn, TxOut CtxUTxO)
genCommittableTxOut =
  Prelude.head . UTxO.pairs <$> (genAdaOnlyUTxO `suchThat` (\u -> length u > 1))

healthyCommitOutput ::
  Party ->
  (TxIn, TxOut CtxUTxO) ->
  (TxIn, (TxOut CtxUTxO, ScriptData))
healthyCommitOutput party committed =
  ( txIn
  ,
    ( toCtxUTxOTxOut (TxOut commitAddress commitValue (mkTxOutDatum commitDatum))
    , fromPlutusData (toData commitDatum)
    )
  )
 where
  txIn = genTxIn `genForParty` party

  (cardanoVk, _) = genKeyPair `genForParty` party

  commitScript =
    fromPlutusScript Commit.validatorScript
  commitAddress =
    mkScriptAddress @PlutusScriptV1 testNetworkId commitScript
  commitValue =
    headValue
      <> (txOutValue . snd) committed
      <> valueFromList
        [ (AssetId testPolicyId (assetNameFromVerificationKey cardanoVk), 1)
        ]
  commitDatum =
    mkCommitDatum party Head.validatorHash (Just committed)

data CollectComMutation
  = MutateOpenUTxOHash
  | MutateHeadScriptInput
  | MutateHeadTransition
  | -- | NOTE: We want to ccheck CollectCom validator checks there's exactly the
    -- expected number of commits. This is needed because the Head protocol
    -- requires to ensure every party has a chance to commit.
    MutateNumberOfParties
  | MutateHeadId
  | MutateRequiredSigner
  deriving (Generic, Show, Enum, Bounded)

genCollectComMutation :: (Tx, UTxO) -> Gen SomeMutation
genCollectComMutation (tx, utxo) =
  oneof
    [ SomeMutation MutateOpenUTxOHash . ChangeOutput 0 <$> mutateUTxOHash
    , SomeMutation MutateHeadScriptInput
        <$> (ChangeInput (headTxIn utxo) <$> anyPayToPubKeyTxOut <*> pure Nothing)
    , SomeMutation MutateHeadTransition <$> do
        changeRedeemer <- ChangeHeadRedeemer <$> (Head.Close 0 . toBuiltin <$> genHash <*> arbitrary)
        changeDatum <- ChangeHeadDatum <$> (Head.Open <$> arbitrary <*> (toBuiltin <$> genHash))
        pure $ Changes [changeRedeemer, changeDatum]
    , SomeMutation MutateNumberOfParties <$> do
        -- NOTE: This also mutates the contestation period becuase we could not
        -- be bothered to decode/lookup the current one.
        c <- arbitrary
        moreParties <- (: healthyOnChainParties) <$> arbitrary
        pure $
          Changes
            [ ChangeHeadDatum $ Head.Initial c moreParties
            , ChangeOutput 0 $ mutatedPartiesHeadTxOut moreParties
            ]
    , SomeMutation MutateHeadId <$> do
        illedHeadResolvedInput <-
          mkHeadOutput
            <$> pure testNetworkId
            <*> fmap headPolicyId (arbitrary `suchThat` (/= testSeedInput))
            <*> pure (toUTxOContext $ mkTxOutDatum healthyCollectComInitialDatum)
        return $ ChangeInput healthyHeadInput illedHeadResolvedInput (Just $ toScriptData Head.CollectCom)
    , SomeMutation MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey
        pure $ ChangeRequiredSigners [newSigner]
    ]
 where
  TxOut collectComOutputAddress collectComOutputValue collectComOutputDatum =
    fromJust $ txOuts' tx !!? 0

  mutatedPartiesHeadTxOut parties =
    -- NOTE / TODO:
    --
    -- This should probably be defined a 'Mutation', but it is different
    -- from 'ChangeHeadDatum'. The latter modifies the _input_ datum given
    -- to the script, whereas this one modifies the resulting head datum in
    -- the output.
    case collectComOutputDatum of
      TxOutDatumNone ->
        error "Unexpected empty head datum"
      (TxOutDatumHash _ha) ->
        error "Unexpected hash-only datum"
      (TxOutDatum sd) ->
        case fromData $ toPlutusData sd of
          (Just Head.Open{utxoHash}) ->
            TxOut
              collectComOutputAddress
              collectComOutputValue
              (mkTxOutDatum Head.Open{parties, utxoHash})
          (Just st) ->
            error $ "Unexpected state " <> show st
          Nothing ->
            error "Invalid data"

  mutateUTxOHash = do
    mutatedUTxOHash <- genHash
    -- NOTE / TODO:
    --
    -- This should probably be defined a 'Mutation', but it is different
    -- from 'ChangeHeadDatum'. The latter modifies the _input_ datum given
    -- to the script, whereas this one modifies the resulting head datum in
    -- the output.
    case collectComOutputDatum of
      TxOutDatumNone ->
        error "Unexpected empty head datum"
      (TxOutDatumHash _ha) ->
        error "Unexpected hash-only datum"
      (TxOutDatum sd) ->
        case fromData $ toPlutusData sd of
          (Just Head.Open{parties}) ->
            pure $
              TxOut
                collectComOutputAddress
                collectComOutputValue
                (mkTxOutDatum Head.Open{parties, Head.utxoHash = toBuiltin mutatedUTxOHash})
          (Just st) ->
            error $ "Unexpected state " <> show st
          Nothing ->
            error "Invalid data"
