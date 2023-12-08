module Hydra.Cardano.Api.TxOutValue where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.MaryEraOnwards (IsMaryEraOnwards (..))

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue ::
  forall era.
  IsMaryEraOnwards era =>
  Value ->
  TxOutValue era
mkTxOutValue =
  TxOutValue (maryEraOnwards @era)
