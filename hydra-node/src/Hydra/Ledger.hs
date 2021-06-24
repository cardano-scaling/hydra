{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger where

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), MockDSIGN, SignKeyDSIGN, VerKeyDSIGN (VerKeyMockDSIGN), signDSIGN)
import Cardano.Crypto.Util (SignableRepresentation)
import Hydra.Prelude hiding (show)

-- NOTE(MB): We probably want to move these common types somewhere else. Putting
-- here to avoid circular dependencies with Hydra.Logic

-- | Identifies a party in a Hydra head.
newtype Party = UnsafeParty (VerKeyDSIGN MockDSIGN)
  deriving (Eq)
  deriving newtype (Show, Read, Num)

deriving instance Read (VerKeyDSIGN MockDSIGN)

instance Ord Party where
  (UnsafeParty a) <= (UnsafeParty b) =
    rawSerialiseVerKeyDSIGN a <= rawSerialiseVerKeyDSIGN b

instance FromCBOR Party where
  fromCBOR = UnsafeParty <$> fromCBOR

instance ToCBOR Party where
  toCBOR (UnsafeParty vk) = toCBOR vk

newtype Signed a = UnsafeSigned (SigDSIGN MockDSIGN)
  deriving (Eq, Show)

instance Typeable a => FromCBOR (Signed a) where
  fromCBOR = UnsafeSigned <$> fromCBOR

instance Typeable a => ToCBOR (Signed a) where
  toCBOR (UnsafeSigned sig) = toCBOR sig

sign :: SignableRepresentation a => SignKeyDSIGN MockDSIGN -> a -> Signed a
sign signingKey signable = UnsafeSigned $ signDSIGN () signable signingKey

-- TODO:
-- deriving instance Read (SigDSIGN MockDSIGN)

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
