{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.TxIn where

import Hydra.Cardano.Api.Prelude

import Cardano.Api (
  BuildTx,
  BuildTxWith (..),
  KeyWitnessInCtx (..),
  Tx,
  TxBodyContent (..),
  TxId (..),
  TxIn (..),
  TxIx (..),
  WitCtxTxIn,
  Witness,
  getTxBody,
  getTxBodyContent,
  getTxId,
 )
import Cardano.Api qualified as Api
import Cardano.Api.Shelley (fromShelleyTxIn, toShelleyTxIn)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary qualified as Ledger
import Cardano.Ledger.Plutus (transSafeHash)
import Cardano.Ledger.TxIn qualified as Ledger
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import PlutusLedgerApi.V3 qualified as Plutus
import Test.QuickCheck (Arbitrary (..), Gen, choose, vectorOf)

-- * Extras

-- | Create a 'TxIn' (a.k.a UTXO) from a transaction and output index.
mkTxIn :: Tx era -> Word -> TxIn
mkTxIn (getTxId . getTxBody -> txId) index =
  TxIn txId (TxIx index)

-- | Attach some verification-key witness to a 'TxIn'.
withWitness :: TxIn -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn Era))
withWitness txIn =
  -- Erik TODO: The qualified `Api.KeyWitness` may not be needed when all
  -- re-exports are removed.
  (txIn, BuildTxWith $ Api.KeyWitness KeyWitnessForSpending)

-- | Access inputs of a transaction, as an ordered list.
txIns' :: Tx era -> [TxIn]
txIns' (getTxBodyContent . getTxBody -> txBodyContent) =
  let TxBodyContent{txIns} = txBodyContent
   in fst <$> txIns

-- | Access inputs of a transaction, as an ordered set.
txInputSet :: Tx era -> Set TxIn
txInputSet = Set.fromList . txIns'

-- * Type Conversions

-- | Convert a cardano-ledger 'TxIn' into a cardano-api 'TxIn'
fromLedgerTxIn :: Ledger.TxIn -> TxIn
fromLedgerTxIn = fromShelleyTxIn

-- | Convert a cardano-api 'TxIn' into a cardano-ledger 'TxIn'
toLedgerTxIn :: TxIn -> Ledger.TxIn
toLedgerTxIn = toShelleyTxIn

-- | Convert a plutus' 'TxOutRef' into a cardano-api 'TxIn'
fromPlutusTxOutRef :: Plutus.TxOutRef -> TxIn
fromPlutusTxOutRef (Plutus.TxOutRef (Plutus.TxId bytes) ix) =
  TxIn
    (TxId $ unsafeHashFromBytes $ Plutus.fromBuiltin bytes)
    (TxIx $ fromIntegral ix)

-- | Convert a cardano-api 'TxIn' into a plutus 'TxOutRef'.
toPlutusTxOutRef :: TxIn -> Plutus.TxOutRef
toPlutusTxOutRef txIn =
  -- XXX: The upstream 'transTxIn' works only with the PlutusV1 type, so we
  -- needed to vendor its definition here.
  let (Ledger.TxIn (Ledger.TxId safe) txIx) = toLedgerTxIn txIn
   in Plutus.TxOutRef (Plutus.TxId $ transSafeHash safe) (toInteger $ Ledger.txIxToInt txIx)

-- * Arbitrary values

-- | A more random generator than the 'Arbitrary TxIn' from cardano-ledger.
-- NOTE: This is using the Cardano ledger's deserialization framework using the
-- latest protocol version via 'maxBound'.
genTxIn :: Gen TxIn
genTxIn =
  fmap fromLedgerTxIn . Ledger.TxIn
    -- NOTE: [88, 32] is a CBOR prefix for a bytestring of 32 bytes.
    <$> fmap (Ledger.unsafeDeserialize' maxBound . BS.pack . ([88, 32] <>)) (vectorOf 32 arbitrary)
    <*> fmap Ledger.TxIx (choose (0, 99))

instance Arbitrary TxIn where
  arbitrary = genTxIn
