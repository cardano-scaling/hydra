-- | The validator used for testing time validity ranges
module Hydra.Contract.Vesting where

import PlutusTx.Prelude

import PlutusLedgerApi.V3 (
  Datum (Datum),
 )
import PlutusLedgerApi.V3 qualified as PlutusV3

-- | Deposit datum containing HeadId, deadline and a list of deposits.
type VestingDatum = (Integer, PlutusV3.PubKeyHash, PlutusV3.PubKeyHash)

datum :: VestingDatum -> PlutusV3.Datum
datum a = Datum (toBuiltinData a)
