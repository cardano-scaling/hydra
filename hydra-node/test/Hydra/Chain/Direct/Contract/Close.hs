{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close where

import Hydra.Prelude hiding (label)

import Cardano.Binary (serialize')
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
 )
import Hydra.Chain.Direct.Tx (
  closeTx,
 )
import Hydra.Chain.Direct.TxSpec (mkHeadOutput)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Data.Party (partyFromVerKey)
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger.Cardano (
  CardanoTx,
  Utxo,
  fromLedgerTx,
  fromLedgerUtxo,
 )
import Hydra.Party (
  MultiSigned (MultiSigned),
  SigningKey,
  deriveParty,
  sign,
  toPlutusSignatures,
  vkey,
 )
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (toData)
import Test.QuickCheck (
  arbitrarySizedNatural,
  oneof,
  suchThat,
 )
import Test.QuickCheck.Instances ()

--
-- CloseTx
--

healthyCloseTx :: (CardanoTx, Utxo)
healthyCloseTx =
  ( fromLedgerTx tx
  , fromLedgerUtxo lookupUtxo
  )
 where
  tx = closeTx healthySnapshot (healthySignature healthySnapshotNumber) (headInput, headOutput, headDatum)
  headInput = generateWith arbitrary 42
  headOutput = mkHeadOutput (SJust headDatum)
  headDatum = Ledger.Data $ toData healthyCloseDatum
  lookupUtxo = Ledger.UTxO $ Map.singleton headInput headOutput

healthySnapshot :: Snapshot CardanoTx
healthySnapshot =
  Snapshot
    { number = healthySnapshotNumber
    , utxo = mempty
    , confirmed = []
    }

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthyCloseDatum :: Head.State
healthyCloseDatum =
  Head.Open
    { parties = healthyCloseParties
    , utxoHash = ""
    }

healthyCloseParties :: [OnChain.Party]
healthyCloseParties = partyFromVerKey . vkey . deriveParty <$> healthyPartyCredentials

healthyPartyCredentials :: [SigningKey]
healthyPartyCredentials = [1, 2, 3]

healthySignature :: SnapshotNumber -> MultiSigned (Snapshot CardanoTx)
healthySignature number = MultiSigned [sign sk snapshot | sk <- healthyPartyCredentials]
 where
  snapshot = healthySnapshot{number}

data CloseMutation
  = MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotToIllFormedValue
  | MutateParties
  deriving (Generic, Show, Enum, Bounded)

genCloseMutation :: (CardanoTx, Utxo) -> Gen SomeMutation
genCloseMutation (_tx, _utxo) =
  -- FIXME: using 'closeRedeemer' here is actually too high-level and reduces
  -- the power of the mutators, we should test at the level of the validator.
  -- That is, using the on-chain types. 'closeRedeemer' is also not used
  -- anywhere after changing this and can be moved into the closeTx
  oneof
    [ SomeMutation MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        closeRedeemer (number healthySnapshot) <$> arbitrary
    , SomeMutation MutateSnapshotNumberButNotSignature . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (\n -> n /= healthySnapshotNumber && n > 0)
        pure (closeRedeemer mutatedSnapshotNumber $ healthySignature healthySnapshotNumber)
    , SomeMutation MutateSnapshotToIllFormedValue . ChangeHeadRedeemer <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (< 0)
        let mutatedSignature =
              MultiSigned [sign sk $ serialize' mutatedSnapshotNumber | sk <- healthyPartyCredentials]
        pure
          Head.Close
            { snapshotNumber = mutatedSnapshotNumber
            , signature = toPlutusSignatures mutatedSignature
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyCloseParties)
        pure $
          Head.Open
            { parties = mutatedParties
            , utxoHash = ""
            }
    , SomeMutation MutateParties . ChangeHeadDatum <$> arbitrary `suchThat` \case
        Head.Open{Head.parties = parties} ->
          parties /= Head.parties healthyCloseDatum
        _ ->
          True
    ]
 where
  closeRedeemer snapshotNumber sig =
    Head.Close
      { snapshotNumber = toInteger snapshotNumber
      , signature = toPlutusSignatures sig
      , utxoHash = ""
      }
