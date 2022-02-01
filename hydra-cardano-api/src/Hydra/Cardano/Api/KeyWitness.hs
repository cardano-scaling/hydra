module Hydra.Cardano.Api.KeyWitness where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import Cardano.Ledger.Keys (KeyRole (Witness))
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger
import qualified Data.Set as Set

-- * Type Conversions

-- | Convert a 'List' of cardano-api's 'KeyWitness' into a 'Set' of
-- cardano-ledger's 'WitVKey'.
--
-- NOTE: 'KeyWitness' is a bigger type than 'WitVKey' witness, this function
-- does not only the type conversion but also the selection of the right
-- underlying constructors. That means the size of the resulting set may be
-- smaller than the size of the list (but never bigger).
toLedgerKeyWitness ::
  [KeyWitness era] ->
  Set (Ledger.WitVKey 'Witness StandardCrypto)
toLedgerKeyWitness vkWits =
  fromList [w | ShelleyKeyWitness _ w <- vkWits]

-- | Convert a 'List' of cardano-api's 'KeyWitness' into a 'Set' of
-- cardano-ledger's 'BootstrapWitness'.
--
-- NOTE: See note on 'toLedgerKeyWitness'.
toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness StandardCrypto)
toLedgerBootstrapWitness vkWits =
  fromList [w | ShelleyBootstrapWitness _ w <- vkWits]

-- | Convert a cardano-ledger's 'TxWitness' object into a list of cardano-api's
-- 'KeyWitness'.
--
-- NOTE: this only concerns key and bootstrap witnesses. Scripts and auxiliary
-- data are obviously not part of the resulting list.
fromLedgerTxWitness ::
  Ledger.TxWitness LedgerEra ->
  [KeyWitness Era]
fromLedgerTxWitness wits =
  Set.foldr ((:) . ShelleyKeyWitness era) [] (Ledger.txwitsVKey' wits)
    ++ Set.foldr ((:) . ShelleyBootstrapWitness era) [] (Ledger.txwitsBoot' wits)
 where
  era =
    ShelleyBasedEraAlonzo
