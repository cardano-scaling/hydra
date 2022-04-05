{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Cardano.Ledger.Alonzo.TxInfo (txInfoOut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Hydra.Cardano.Api (
  UTxO,
  toLedgerTxOut,
 )
import Hydra.Chain.Direct.Contract.Abort (genAbortMutation, healthyAbortTx, propHasCommit, propHasInitial)
import Hydra.Chain.Direct.Contract.Close (genCloseMutation, healthyCloseTx)
import Hydra.Chain.Direct.Contract.CollectCom (genCollectComMutation, healthyCollectComTx)
import Hydra.Chain.Direct.Contract.Commit (genCommitMutation, healthyCommitTx)
import Hydra.Chain.Direct.Contract.FanOut (genFanoutMutation, healthyFanoutTx)
import Hydra.Chain.Direct.Contract.Init (genHealthyIdleSt, genInitMutation, genObserveInitMutation, healthyInitTx)
import Hydra.Chain.Direct.Contract.Mutation (
  genListOfSigningKeys,
  propMutationOffChain,
  propMutationOnChain,
  propTransactionValidates,
 )
import Hydra.Chain.Direct.State (SomeOnChainHeadState (..))
import Hydra.Contract.Encoding (serialiseTxOuts)
import Hydra.Contract.Head (
  verifyPartySignature,
  verifySnapshotSignature,
 )
import qualified Hydra.Contract.Head as Head
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  genUTxOWithSimplifiedAddresses,
  hashTxOuts,
  shrinkUTxO,
 )
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (
  Signed (UnsafeSigned),
  aggregate,
  generateKey,
  sign,
  toPlutusSignatures,
 )
import Hydra.Snapshot (Snapshot (..))
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Crypto (Signature (Signature))
import Test.QuickCheck (
  Positive (Positive),
  Property,
  conjoin,
  counterexample,
  forAll,
  forAllShrink,
  (===),
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = parallel $ do
  describe "Signature validator" $ do
    prop
      "verifies single signature produced off-chain"
      prop_verifyOffChainSignatures
    -- FIXME(AB): This property exists solely because our current multisignature implementation
    -- is just the aggregates of individual (mock) signatures and there is no point in doing some
    -- complicated shuffle logic to verify signatures given we'll end up verifying a single Ed25519
    -- signatures.
    prop
      "verifies snapshot multi-signature for list of parties and signatures"
      prop_verifySnapshotSignatures
  describe "TxOut hashing" $ do
    modifyMaxSuccess (const 20) $
      prop "OffChain.hashTxOuts == OnChain.hashTxOuts" prop_consistentOnAndOffChainHashOfTxOuts

  describe "Init" $ do
    prop "is healthy" $
      propTransactionValidates healthyInitTx
    prop "does not survive random adversarial mutations (on-chain)" $
      propMutationOnChain healthyInitTx genInitMutation
    prop "does not survive random adversarial mutations (off-chain)" $
      propMutationOffChain healthyInitTx genObserveInitMutation (SomeOnChainHeadState <$> genHealthyIdleSt)

  describe "Abort" $ do
    prop "is healthy" $
      conjoin
        [ propTransactionValidates healthyAbortTx
        , propHasCommit healthyAbortTx
        , propHasInitial healthyAbortTx
        ]
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyAbortTx genAbortMutation
  describe "Commit" $ do
    prop "is healthy" $
      propTransactionValidates healthyCommitTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyCommitTx genCommitMutation
  describe "CollectCom" $ do
    prop "is healthy" $
      propTransactionValidates healthyCollectComTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyCollectComTx genCollectComMutation
  describe "Close" $ do
    prop "is healthy" $
      propTransactionValidates healthyCloseTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyCloseTx genCloseMutation
  describe "Fanout" $ do
    prop "is healthy" $
      propTransactionValidates healthyFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyFanoutTx genFanoutMutation

--
-- Properties
--

prop_consistentOnAndOffChainHashOfTxOuts :: Property
prop_consistentOnAndOffChainHashOfTxOuts =
  -- NOTE: We only generate shelley addressed txouts because they are left out
  -- of the plutus script context in 'txInfoOut'.
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let plutusTxOuts = mapMaybe (txInfoOut . toLedgerTxOut) ledgerTxOuts
        ledgerTxOuts = toList utxo
        plutusBytes = serialiseTxOuts plutusTxOuts
        ledgerBytes = serialize' (toLedgerTxOut <$> ledgerTxOuts)
     in (hashTxOuts ledgerTxOuts === fromBuiltin (Head.hashTxOuts plutusTxOuts))
          & counterexample ("Plutus: " <> show plutusTxOuts)
          & counterexample ("Ledger: " <> show ledgerTxOuts)
          & counterexample ("Ledger CBOR: " <> decodeUtf8 (Base16.encode ledgerBytes))
          & counterexample ("Plutus CBOR: " <> decodeUtf8 (Base16.encode $ fromBuiltin plutusBytes))

prop_verifyOffChainSignatures :: Property
prop_verifyOffChainSignatures =
  forAll arbitrary $ \(snapshot :: Snapshot SimpleTx) ->
    forAll arbitrary $ \(Positive n) ->
      let sk = generateKey n
          UnsafeSigned signature = sign sk snapshot
          party = partyFromVerKey $ deriveVerKeyDSIGN sk
          snapshotNumber = toInteger $ number snapshot
       in verifyPartySignature snapshotNumber party (Signature $ toBuiltin signature)
            & counterexample ("signed: " <> show (BS.unpack signature))
            & counterexample ("party: " <> show party)
            & counterexample ("message: " <> show (getSignableRepresentation snapshot))

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot :: Snapshot SimpleTx) ->
    forAll genListOfSigningKeys $ \sks ->
      let parties = partyFromVerKey . deriveVerKeyDSIGN <$> sks
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger $ number snapshot
       in verifySnapshotSignature parties snapshotNumber signatures
