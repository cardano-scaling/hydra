-- | Structured errors related to configuration mismatch.
--
-- When we start a `Hydra.Node` we need to do sanity checks between what's
-- provided as parameters to the node and what's persisted.
module Hydra.Node.ParameterMismatch where

import Hydra.Prelude

import Hydra.Tx (Party)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)

-- | Exception used to indicate command line options not matching the persisted
-- state.
newtype ParameterMismatch = ParameterMismatch [ParamMismatch]
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ParamMismatch
  = ContestationPeriodMismatch {loadedCp :: ContestationPeriod, configuredCp :: ContestationPeriod}
  | PartiesMismatch {loadedParties :: [Party], configuredParties :: [Party]}
  | SavedNetworkPartiesInconsistent {numberOfParties :: Int}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

instance ToCBOR ParamMismatch where
  toCBOR = \case
    ContestationPeriodMismatch{loadedCp, configuredCp} ->
      toCBOR ("ContestationPeriodMismatch" :: Text) <> toCBOR loadedCp <> toCBOR configuredCp
    PartiesMismatch{loadedParties, configuredParties} ->
      toCBOR ("PartiesMismatch" :: Text) <> toCBOR loadedParties <> toCBOR configuredParties
    SavedNetworkPartiesInconsistent{numberOfParties} ->
      toCBOR ("SavedNetworkPartiesInconsistent" :: Text) <> toCBOR numberOfParties

instance FromCBOR ParamMismatch where
  fromCBOR =
    fromCBOR >>= \case
      ("ContestationPeriodMismatch" :: Text) -> ContestationPeriodMismatch <$> fromCBOR <*> fromCBOR
      "PartiesMismatch" -> PartiesMismatch <$> fromCBOR <*> fromCBOR
      "SavedNetworkPartiesInconsistent" -> SavedNetworkPartiesInconsistent <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded ParamMismatch"
