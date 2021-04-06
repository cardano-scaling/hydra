module Hydra.OnChainTransaction.State where

import Cardano.Prelude

-- TODO: Remove dependency on types from contract
import Hydra.Contract.Types (
  Eta (..),
  HydraState (..),
  MultisigPublicKey (MultisigPublicKey),
  OpenState (..),
  UTXO (..),
  VerificationKey,
 )

initialState :: [VerificationKey] -> HydraState
initialState keys = Open openState
 where
  openState =
    OpenState
      { keyAggregate = MultisigPublicKey keys
      , eta =
          Eta
            { utxos = UTXO
            , snapshotNumber = 0
            , transactions = mempty
            }
      }
