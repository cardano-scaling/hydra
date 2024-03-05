module Hydra.Cardano.Api.TxOutValue where

import Hydra.Cardano.Api.Prelude

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue ::
  forall era.
  IsShelleyBasedEra era =>
  IsMaryEraOnwards era =>
  Value ->
  TxOutValue era
mkTxOutValue v =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $ TxOutValueShelleyBased (shelleyBasedEra @era) (toLedgerValue (maryEraOnwards @era) v)
