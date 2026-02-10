module Hydra.Cardano.Api.TxOutValue where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue ::
  forall era.
  IsMaryBasedEra era =>
  Value ->
  TxOutValue era
mkTxOutValue v =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $ TxOutValueShelleyBased (shelleyBasedEra @era) (toLedgerValue (maryBasedEra @era) v)
