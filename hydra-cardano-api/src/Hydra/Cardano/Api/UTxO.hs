{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | NOTE: This module is meant to be imported qualified as 'UTxO'.
module Hydra.Cardano.Api.UTxO where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxId (toLedgerTxId)
import Hydra.Cardano.Api.TxIn (fromLedgerTxIn, toLedgerTxIn)
import Hydra.Cardano.Api.TxOut (fromLedgerTxOut, toLedgerTxOut)

import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Data.Map as Map
import qualified Data.Text as T

type UTxO = UTxO' (TxOut CtxUTxO Era)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO'and provide 'Monoid' and 'Foldable' instances to make utxo
-- manipulation bareable.
newtype UTxO' out = UTxO
  { toMap :: Map TxIn out
  }
  deriving newtype
    ( Eq
    , Show
    , Functor
    , Foldable
    , Semigroup
    , Monoid
    )

instance Traversable UTxO' where
  traverse fn (UTxO m) = UTxO <$> traverse fn m

instance ToCBOR UTxO where
  toCBOR = toCBOR . toLedgerUTxO
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR UTxO where
  fromCBOR = fromLedgerUTxO <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

-- | Create a 'UTxO' from a single unspent transaction output.
singleton :: (TxIn, TxOut CtxUTxO Era) -> UTxO
singleton (i, o) = UTxO $ Map.singleton i o

-- | Turn a 'UTxO' into a list of pairs.
pairs :: UTxO' out -> [(TxIn, out)]
pairs = Map.toList . toMap

-- | Construct a UTxO from a transaction. This constructs artificial `TxIn`
-- (a.k.a output reference) from the transaction itself, zipping them to the
-- outputs they correspond to.
fromTx :: Tx Era -> UTxO
fromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ Ledger.outputs' ledgerBody
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [0 .. fromIntegral (length txOuts)]
        ]
   in fromLedgerUTxO $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- | Get a human-readable pretty text representation of a UTxO.
render :: (TxIn, TxOut ctx era) -> Text
render (k, TxOut _ (txOutValueToValue -> v) _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> renderValue v

-- | See 'render'.
renderUTxO :: (TxIn, TxOut ctx era) -> Text
renderUTxO = render

-- * Type Conversions

toLedgerUTxO :: UTxO -> Ledger.UTxO LedgerEra
toLedgerUTxO =
  Ledger.UTxO . Map.foldMapWithKey fn . toMap
 where
  fn ::
    TxIn ->
    TxOut CtxUTxO Era ->
    Map (Ledger.TxIn StandardCrypto) (Ledger.TxOut LedgerEra)
  fn i o =
    Map.singleton (toLedgerTxIn i) (toLedgerTxOut o)

fromLedgerUTxO :: Ledger.UTxO LedgerEra -> UTxO
fromLedgerUTxO =
  UTxO . Map.foldMapWithKey fn . Ledger.unUTxO
 where
  fn ::
    Ledger.TxIn StandardCrypto ->
    Ledger.TxOut LedgerEra ->
    Map TxIn (TxOut CtxUTxO Era)
  fn i o =
    Map.singleton (fromLedgerTxIn i) (fromLedgerTxOut o)
