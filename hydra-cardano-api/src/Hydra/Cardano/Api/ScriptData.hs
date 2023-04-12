{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ScriptData where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Api as Api
import Cardano.Api.Byron (TxBody (..))
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Arrow (left)
import Data.Aeson (Value (String))
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import qualified PlutusLedgerApi.V2 as Plutus

-- * Extras

-- | Data-types that can be marshalled into a generic 'ScriptData' structure.
type ToScriptData a = Plutus.ToData a

-- | Data-types that can be unmarshalled from a generic 'ScriptData' structure.
type FromScriptData a = Plutus.FromData a

-- | Serialise some type into a generic script data.
toScriptData :: (ToScriptData a) => a -> HashableScriptData
toScriptData =
  unsafeHashableScriptData . fromPlutusData . Plutus.toData

-- | Get the 'ScriptData' associated to the a 'TxOut'. Note that this requires
-- the 'CtxTx' context. To get script data in a 'CtxUTxO' context, see
-- 'lookupScriptData'.
getScriptData :: TxOut CtxTx era -> Maybe ScriptData
getScriptData (TxOut _ _ d _) =
  case d of
    TxOutDatumInTx _ sd -> Just $ Api.getScriptData sd
    TxOutDatumInline _ sd -> Just $ Api.getScriptData sd
    _ -> Nothing

-- | Lookup included datum of given 'TxOut'.
lookupScriptData ::
  forall era.
  ( UsesStandardCrypto era
  , Typeable (ShelleyLedgerEra era)
  ) =>
  Tx era ->
  TxOut CtxUTxO era ->
  Maybe ScriptData
lookupScriptData (Tx ByronTxBody{} _) _ = Nothing
lookupScriptData (Tx (ShelleyTxBody _ _ _ scriptsData _ _) _) (TxOut _ _ datum _) =
  case datum of
    TxOutDatumNone ->
      Nothing
    (TxOutDatumHash _ (ScriptDataHash h)) ->
      fromPlutusData . Ledger.getPlutusData <$> Map.lookup h datums
    (TxOutDatumInline _ dat) ->
      Just $ Api.getScriptData dat
 where
  datums = case scriptsData of
    TxBodyNoScriptData -> mempty
    TxBodyScriptData _ (Ledger.TxDats m) _ -> m

-- * Type Conversions


-- | Convert a cardano-ledger script 'Data' into a cardano-api 'ScriptDatum'.
-- TODO: check whether we should use HashableScriptData instead
fromLedgerData :: Ledger.Data era -> ScriptData
fromLedgerData =
  Api.getScriptData . fromAlonzoData

-- | Convert a cardano-api script data into a cardano-ledger script 'Data'.
toLedgerData :: HashableScriptData -> Ledger.Data era
toLedgerData =
  toAlonzoData

-- * Orphans

instance ToJSON ScriptData where
  toJSON =
    String
      . decodeUtf8
      . Base16.encode
      . toStrict
      . serialise
      . toPlutusData

instance FromJSON ScriptData where
  parseJSON v = do
    text :: Text <- parseJSON v
    either fail (pure . fromPlutusData) $ do
      bytes <- Base16.decode (encodeUtf8 text)
      left show $ deserialiseOrFail $ fromStrict bytes
