module Hydra.Cardano.Api.Value where

import Hydra.Cardano.Api.Prelude

txOutValue :: TxOut ctx Era -> Value
txOutValue (TxOut _ value _) =
  txOutValueToValue value
