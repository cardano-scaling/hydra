{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Ledger where

import Hydra.Prelude

import Data.Aeson (object, withObject, (.:), (.:?), (.=))
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.UserValidationRules (PolicyViolation, UserValidation)

-- | An abstract interface for a 'Ledger'. Allows to define mock / simpler
-- implementation for testing as well as limiting feature-envy from the business
-- logic by forcing a closed interface.
data Ledger tx = Ledger
  { applyTransactions ::
      ChainSlot ->
      UTxOType tx ->
      [tx] ->
      Either (tx, ValidationError) (UTxOType tx)
  -- ^ Apply a set of transaction to a given UTxO set. Returns the new UTxO or
  -- validation failures returned from the ledger.
  -- TODO: 'ValidationError' should also include the UTxO, which is not
  -- necessarily the same as the given UTxO after some transactions
  , userValidation :: Maybe (UserValidation tx)
  -- ^ potential user policy validation if set on start
  }

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Show, Generic)

data ValidationError
  = ValidationError {reason :: Text}
  | PolicyValidationError {policyViolation :: PolicyViolation}
  deriving stock (Eq, Show, Generic)

-- Keep the existing ledger-error JSON unchanged for clients and checkpoints.
instance ToJSON ValidationError where
  toJSON = \case
    ValidationError reason -> object ["reason" .= reason]
    PolicyValidationError violation ->
      object ["tag" .= ("PolicyValidationError" :: Text), "policyViolation" .= violation]

instance FromJSON ValidationError where
  parseJSON = withObject "ValidationError" $ \o -> do
    tag <- o .:? "tag"
    case tag :: Maybe Text of
      Nothing -> ValidationError <$> o .: "reason"
      Just "PolicyValidationError" -> PolicyValidationError <$> o .: "policyViolation"
      Just unknown -> fail $ "Unknown validation error: " <> show unknown

renderValidationError :: ValidationError -> Text
renderValidationError = \case
  ValidationError reason -> reason
  PolicyValidationError violation -> show violation

instance ToCBOR ValidationError where
  toCBOR = genericToCBOR

instance FromCBOR ValidationError where
  fromCBOR = genericFromCBOR
