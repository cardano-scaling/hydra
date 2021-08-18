module Hydra.Ledger.Cardano where

import Hydra.Prelude

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Hydra.Ledger (Tx (..))
import qualified Shelley.Spec.Ledger.API as Cardano

data CardanoTx

instance Tx CardanoTx where
  type Utxo CardanoTx = Cardano.UTxO (MaryEra StandardCrypto)
  type TxId CardanoTx = Cardano.TxId StandardCrypto

  txId tx = Cardano.TxId $ hashAnnotated body
   where
    body = (undefined :: CardanoTx -> Cardano.TxBody (MaryEra StandardCrypto)) tx
