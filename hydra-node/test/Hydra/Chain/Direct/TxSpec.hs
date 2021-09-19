{-# LANGUAGE TypeApplications #-}
-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo.Data (Data (Data))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, wits))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txdats), nullDats, unTxDats)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Hydra.Chain (HeadParameters (..), toOnChainTx)
import Hydra.Chain.Direct.Tx (initTx, observeTx, constructTx)
import Hydra.Contract.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Contract.Head (State (Initial))
import Hydra.Contract.Party (partyFromVerKey)
import Hydra.Party (vkey)
import Plutus.V1.Ledger.Api (toBuiltinData, toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (counterexample, (===))
import Hydra.Ledger.Simple (SimpleTx)

spec :: Spec
spec =
  parallel $ do
    prop "observeTx . constructTx rountrip" $ \postTx txIn time ->
      observeTx (constructTx txIn postTx) === Just (toOnChainTx @SimpleTx time postTx)

    describe "initTx" $ do
      prop "can construct & serialize unsigned initTx" $ \txIn params ->
        let tx = initTx params txIn
            cbor = serialize tx
            len = LBS.length cbor
         in counterexample ("Tx: " <> show tx) $
              counterexample ("Tx serialized size: " <> show len) $
                len < 16000 -- TODO(SN): use real max tx size

      prop "contains some datums" $ \txIn params ->
        let ValidatedTx{wits} = initTx params txIn
            dats = txdats wits
         in counterexample ("TxDats: " <> show dats) $
              not $ nullDats dats

      prop "contains HeadParameters as datums" $ \txIn params ->
        let ValidatedTx{wits} = initTx params txIn
            dats = txdats wits
            HeadParameters{contestationPeriod, parties} = params
            onChainPeriod = contestationPeriodFromDiffTime contestationPeriod
            onChainParties = map (partyFromVerKey . vkey) parties
            datum = Initial onChainPeriod onChainParties
         in Map.elems (unTxDats dats) === [Data . toData $ toBuiltinData datum]
