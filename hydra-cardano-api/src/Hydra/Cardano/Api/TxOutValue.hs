module Hydra.Cardano.Api.TxOutValue where

import Cardano.Api (IsMaryBasedEra, TxOutValue (..), Value, maryBasedEra, shelleyBasedEra, shelleyBasedEraConstraints, toLedgerValue)

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue ::
  forall era.
  IsMaryBasedEra era =>
  Value ->
  TxOutValue era
mkTxOutValue v =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $ TxOutValueShelleyBased (shelleyBasedEra @era) (toLedgerValue (maryBasedEra @era) v)
