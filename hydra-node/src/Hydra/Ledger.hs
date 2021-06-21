module Hydra.Ledger where

import Hydra.Prelude hiding (show)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (VerKeyDSIGN, rawSerialiseVerKeyDSIGN), MockDSIGN, encodeVerKeyDSIGN, decodeVerKeyDSIGN)
import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import Text.Read (Read(..))

-- NOTE(MB): We probably want to move these common types somewhere else. Putting
-- here to avoid circular dependencies with Hydra.Logic

-- | Identifies a party in a Hydra head.
newtype Party = UnsafeParty (VerKeyDSIGN MockDSIGN)
  deriving (Eq, Show)
  deriving newtype Num

instance Ord Party where
  (UnsafeParty a) <= (UnsafeParty b) =
    rawSerialiseVerKeyDSIGN a <= rawSerialiseVerKeyDSIGN b

instance Read Party where
  readsPrec = error "TODO: use json or cbor instead"

instance FromCBOR Party where
  fromCBOR = UnsafeParty <$> decodeVerKeyDSIGN

instance ToCBOR Party where
  toCBOR (UnsafeParty vk) = encodeVerKeyDSIGN vk

type Committed tx = Map Party (UTxO tx)

-- * Ledger interface

class
  ( Eq tx
  , Eq (UTxO tx)
  , Show tx
  , Show (UTxO tx)
  , Read tx
  , Read (UTxO tx)
  , Monoid (UTxO tx)
  , Typeable tx
  ) =>
  Tx tx
  where
  type UTxO tx

data Ledger tx = Ledger
  { applyTransactions :: UTxO tx -> [tx] -> Either ValidationError (UTxO tx)
  , initUTxO :: UTxO tx
  }

canApply :: Ledger tx -> UTxO tx -> tx -> ValidationResult
canApply ledger utxo tx =
  either Invalid (const Valid) $ applyTransactions ledger utxo (pure tx)

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving (Eq, Show)

data ValidationError = ValidationError deriving (Eq, Show)
