-- | NOTE (1): This module is meant to be imported qualified as 'UTxO'.
--
--   NOTE (2): This module is name-spaces slightly different from the rest
--   because it is meant to be used as a replacement of the UTxO type of the
--   cardano-api which is not convenient enough to work with. Having it as
--   'Hydra.Cardano.Api.UTxO' causes cyclic imports with other modules also
--   relying on this newtype. So instead, we do 'as if' it was part of the
--   cardano-api in the first palce.
module Cardano.Api.UTxO where

import Hydra.Prelude

import Cardano.Api hiding (UTxO, toLedgerUTxO)
import qualified Cardano.Api
import qualified Data.Map as Map
import qualified Data.Text as T

type UTxO = UTxO' (TxOut CtxUTxO AlonzoEra)

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

-- | Create a 'UTxO' from a list of 'TxIn' and 'out' pairs.
fromPairs :: [(TxIn, out)] -> UTxO' out
fromPairs = UTxO . Map.fromList

-- | Create a 'UTxO' from a single unspent transaction output.
singleton :: (TxIn, TxOut CtxUTxO AlonzoEra) -> UTxO
singleton (i, o) = UTxO $ Map.singleton i o

-- | Find an 'out' for a given 'TxIn'.
resolve :: TxIn -> UTxO' out -> Maybe out
resolve k = Map.lookup k . toMap

-- | Turn a 'UTxO' into a list of pairs.
pairs :: UTxO' out -> [(TxIn, out)]
pairs = Map.toList . toMap

-- | Get the 'UTxO' domain input's set
inputSet :: UTxO' out -> Set TxIn
inputSet = Map.keysSet . toMap

-- | Get a human-readable pretty text representation of a UTxO.
render :: (TxIn, TxOut ctx era) -> Text
render (k, TxOut _ (txOutValueToValue -> v) _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> renderValue v

-- | Select the minimum (by TxIn) utxo entry from the UTxO map.
--
-- This function is partial.
min :: UTxO -> UTxO
min = UTxO . uncurry Map.singleton . Map.findMin . toMap

-- * Type Conversions

fromApi :: Cardano.Api.UTxO AlonzoEra -> UTxO
fromApi = coerce

toApi :: UTxO -> Cardano.Api.UTxO AlonzoEra
toApi = coerce
