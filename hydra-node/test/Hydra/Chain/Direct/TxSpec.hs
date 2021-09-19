{-# LANGUAGE TypeApplications #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo.Data (Data (Data))
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, wits, body), outputs)
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
import Hydra.Chain.Direct (Era)
import Cardano.Ledger.Mary.Value (Value(Value), PolicyID, AssetName)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.TxBody (TxOut(TxOut))
import Cardano.Ledger.Crypto (StandardCrypto)

spec :: Spec
spec =
  parallel $ do
    prop "observeTx . constructTx roundtrip" $ \postTx txIn time ->
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

      -- TODO(SN): assert monetary policy?
      prop "distributes participation tokens" $ \txIn params ->
        let ValidatedTx{body} = initTx params txIn
            nfts = foldMap txOutNFT $ outputs body
         in counterexample ("NFTs: " <> show nfts) $
            length nfts == length (parties params)

-- | Extract NFT candidates. any single quantity assets not being ADA is a
-- candidate.
txOutNFT :: TxOut Era -> [(PolicyID StandardCrypto, AssetName)]
txOutNFT (TxOut _ value _ ) =
  mapMaybe findUnitAssets $ Map.toList assets
 where
  (Value _ assets) = value

  findUnitAssets (policy, as) = do
    (name, _q) <- find unitQuantity $ Map.toList as
    pure (policy, name)

  unitQuantity (_name,q) = q == 1
