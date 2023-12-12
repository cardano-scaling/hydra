-- | NOTE (1): This module is meant to be imported qualified as 'UTxO'.
--
--   NOTE (2): This module is name-spaces slightly different from the rest
--   because it is meant to be used as a replacement of the UTxO type of the
--   cardano-api which is not convenient enough to work with. Having it as
--   'Hydra.Cardano.Api.UTxO' causes cyclic imports with other modules also
--   relying on this newtype. So instead, we do 'as if' it was part of the
--   cardano-api in the first palce.
module Cardano.Api.UTxO where

import Cardano.Api hiding (UTxO, toLedgerUTxO)
import Cardano.Api qualified
import Cardano.Api.Class.IsAlonzoEraOnwards (IsAlonzoEraOnwards (..))
import Cardano.Api.Class.IsBabbageEraOnwards (IsBabbageEraOnwards (..))
import Cardano.Api.Class.IsMaryEraOnwards (IsMaryEraOnwards (..))
import Cardano.Api.Shelley (ReferenceScript (..))
import Cardano.Ledger.Babbage ()
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

type Era = BabbageEra

type UTxO = UTxO' (TxOut CtxUTxO Era)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO' and provide 'Monoid' and 'Foldable' instances to make utxo
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
    , ToJSON
    , FromJSON
    )

instance Traversable UTxO' where
  traverse fn (UTxO m) = UTxO <$> traverse fn m

-- | Create a 'UTxO' from a list of 'TxIn' and 'out' pairs.
fromPairs :: [(TxIn, out)] -> UTxO' out
fromPairs = UTxO . Map.fromList

-- | Create a 'UTxO' from a single unspent transaction output.
singleton :: (TxIn, out) -> UTxO' out
singleton (i, o) = UTxO $ Map.singleton i o

-- | Find an 'out' for a given 'TxIn'.
resolve :: TxIn -> UTxO' out -> Maybe out
resolve k = Map.lookup k . toMap

-- | Turn a 'UTxO' into a list of pairs.
pairs :: UTxO' out -> [(TxIn, out)]
pairs = Map.toList . toMap

-- | Find first 'UTxO' which satisfies given predicate.
find :: (out -> Bool) -> UTxO' out -> Maybe (TxIn, out)
find fn utxo = List.find (fn . snd) $ pairs utxo

-- | Filter UTxO to only include 'out's satisfying given predicate.
filter :: (out -> Bool) -> UTxO' out -> UTxO' out
filter fn = UTxO . Map.filter fn . toMap

-- | Get the 'UTxO' domain input's set
inputSet :: UTxO' out -> Set TxIn
inputSet = Map.keysSet . toMap

-- | Get a human-readable pretty text representation of a UTxO.
render :: (TxIn, TxOut ctx era) -> Text
render (k, TxOut _ (txOutValueToValue -> v) _ _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> renderValue v

-- | Select the minimum (by TxIn) utxo entry from the UTxO map.
--
-- This function is partial.
min :: UTxO -> UTxO
min = UTxO . uncurry Map.singleton . Map.findMin . toMap

-- | Remove the right hand side from the left hand side.
difference :: UTxO' out -> UTxO' out -> UTxO' out
difference a b = UTxO $ Map.difference (toMap a) (toMap b)

-- | Infix version of 'difference'.
(\\) :: UTxO' out -> UTxO' out -> UTxO' out
a \\ b = difference a b

-- * Type Conversions

-- | Transforms a UTxO containing tx outs from any era into Babbage era.
fromApi :: Cardano.Api.UTxO era -> UTxO
fromApi (Cardano.Api.UTxO eraUTxO) =
  fromPairs $ second convertOutputToEra <$> Map.toList eraUTxO
 where
  -- NOTE: At latest the TxOutValue is an existential where we need to case on
  -- the 'sbe' witness to get constraints on the contained 'value', but the
  -- 'cardano-api' does that already when allowing conversion of their
  -- (complicated) constrained types to the cardano-ledger types - so we just
  -- convert forth and back.
  convertOutputToEra :: TxOut CtxUTxO era -> TxOut CtxUTxO Era
  convertOutputToEra (TxOut eraAddress eraValue eraDatum eraRefScript) =
    TxOut
      (convertAddressToEra eraAddress)
      (convertValueToEra eraValue)
      (convertDatumToEra eraDatum)
      (convertRefScriptToEra eraRefScript)

  convertAddressToEra :: AddressInEra era -> AddressInEra Era
  convertAddressToEra (AddressInEra _ eraAddress) = anyAddressInShelleyBasedEra shelleyBasedEra (toAddressAny eraAddress)

  convertValueToEra :: TxOutValue era -> TxOutValue Era
  convertValueToEra (TxOutValueByron lovelace) = lovelaceToTxOutValue shelleyBasedEra lovelace
  convertValueToEra (TxOutValueShelleyBased sbe value) = TxOutValueShelleyBased shelleyBasedEra (toLedgerValue (maryEraOnwards @Era) $ fromLedgerValue sbe value)

  convertDatumToEra :: TxOutDatum CtxUTxO era -> TxOutDatum CtxUTxO Era
  convertDatumToEra TxOutDatumNone = TxOutDatumNone
  convertDatumToEra (TxOutDatumHash _ hashScriptData) = TxOutDatumHash alonzoEraOnwards hashScriptData
  convertDatumToEra (TxOutDatumInline _ hashableScriptData) = TxOutDatumInline babbageEraOnwards hashableScriptData

  convertRefScriptToEra :: ReferenceScript era -> ReferenceScript Era
  convertRefScriptToEra ReferenceScriptNone = ReferenceScriptNone
  convertRefScriptToEra (ReferenceScript _ scriptInAnyLang) = ReferenceScript babbageEraOnwards scriptInAnyLang

toApi :: UTxO -> Cardano.Api.UTxO Era
toApi = coerce
