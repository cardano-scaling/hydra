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
import Cardano.Api.Shelley (ReferenceScript (..))
import Cardano.Ledger.Babbage ()
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

type Era = ConwayEra

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

-- | Create a 'UTxO' from a list of 'TxIn' and 'out' pairs.
fromList :: [(TxIn, out)] -> UTxO' out
fromList = UTxO . Map.fromList

-- | Create a 'UTxO' from a single unspent transaction output.
singleton :: TxIn -> out -> UTxO' out
singleton i o = UTxO $ Map.singleton i o

-- | Find an 'out' for a given 'TxIn'.
resolveTxIn :: TxIn -> UTxO' out -> Maybe out
resolveTxIn k = Map.lookup k . toMap

-- | Turn a 'UTxO' into a list of pairs.
toList :: UTxO' out -> [(TxIn, out)]
toList = Map.toList . toMap

-- | Find first 'UTxO' using the output in predicate.
find :: (out -> Bool) -> UTxO' out -> Maybe (TxIn, out)
find fn = findBy (fn . snd)

-- | Find first 'UTxO' using both input and output in predicate.
findBy :: ((TxIn, out) -> Bool) -> UTxO' out -> Maybe (TxIn, out)
findBy fn utxo = List.find fn $ toList utxo

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

-- | Remove the right hand side from the left hand side.
difference :: UTxO' out -> UTxO' out -> UTxO' out
difference a b = UTxO $ Map.difference (toMap a) (toMap b)

-- | Check if the first 'UTxO' contains all **outputs** of the second 'UTxO'.
-- First argument is the 'UTxO' to search in, second argument is the 'UTxO'
-- to search for.
containsOutputs :: Eq out => UTxO' out -> UTxO' out -> Bool
containsOutputs utxoForSearching utxo =
  let allOutputs = F.toList utxoForSearching
      expectedOutputs = F.toList utxo
   in all (`elem` allOutputs) expectedOutputs

-- * Type Conversions

-- | Transforms a UTxO containing tx outs from any era into Babbage era.
fromApi :: Cardano.Api.UTxO era -> UTxO
fromApi (Cardano.Api.UTxO eraUTxO) =
  fromList $ second convertOutputToEra <$> Map.toList eraUTxO
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
  convertValueToEra (TxOutValueShelleyBased sbe value) = TxOutValueShelleyBased shelleyBasedEra (toLedgerValue (maryBasedEra @Era) $ fromLedgerValue sbe value)

  convertDatumToEra :: TxOutDatum CtxUTxO era -> TxOutDatum CtxUTxO Era
  convertDatumToEra TxOutDatumNone = TxOutDatumNone
  convertDatumToEra (TxOutDatumHash _ hashScriptData) = TxOutDatumHash alonzoBasedEra hashScriptData
  convertDatumToEra (TxOutDatumInline _ hashableScriptData) = TxOutDatumInline babbageBasedEra hashableScriptData

  convertRefScriptToEra :: ReferenceScript era -> ReferenceScript Era
  convertRefScriptToEra ReferenceScriptNone = ReferenceScriptNone
  convertRefScriptToEra (ReferenceScript _ scriptInAnyLang) = ReferenceScript babbageBasedEra scriptInAnyLang

toApi :: UTxO -> Cardano.Api.UTxO Era
toApi = coerce
