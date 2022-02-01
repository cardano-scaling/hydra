module Hydra.Cardano.Api.KeyWitness where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Keys (KeyRole (Witness))
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger

toLedgerKeyWitness ::
  [KeyWitness era] ->
  Set (Ledger.WitVKey 'Witness StandardCrypto)
toLedgerKeyWitness vkWits =
  fromList [w | ShelleyKeyWitness _ w <- vkWits]

toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness StandardCrypto)
toLedgerBootstrapWitness vkWits =
  fromList [w | ShelleyBootstrapWitness _ w <- vkWits]
