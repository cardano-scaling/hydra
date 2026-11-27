module Hydra.Tx.ContestationPeriod.Types where

import Hydra.Prelude hiding (Show, show)

import Data.Fixed (Pico)
import Data.Time (secondsToNominalDiffTime)
import Text.Show (Show (..))

-- | A positive, non-zero number of seconds. Use 'fromInteger' on positive
-- literals, 'fromEnum' via [1..] syntax or 'fromNominalDiffTime' to create
-- values of unknown sign.
-- NOTE: 'FromJSON' is deliberately newtype-derived and therefore total: it also
-- decodes the persisted event log, where a value an older node legitimately
-- wrote has to stay decodable or the node could never start again. A configured
-- period is validated at the configuration boundary instead, see
-- 'Hydra.Config.parseCardanoChainConfig'.
newtype ContestationPeriod = UnsafeContestationPeriod Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Real, Integral, ToJSON, FromJSON)

instance ToCBOR ContestationPeriod where
  toCBOR = genericToCBOR

instance FromCBOR ContestationPeriod where
  fromCBOR = genericFromCBOR

instance Show ContestationPeriod where
  show (UnsafeContestationPeriod s) = show s <> "s"

instance Enum ContestationPeriod where
  toEnum i
    | i > 0 = UnsafeContestationPeriod $ toEnum i
    | otherwise = error $ "ContestationPeriod.toEnum: " <> toText (show i) <> " is not > 0"

  fromEnum (UnsafeContestationPeriod n) = fromEnum n

instance Num ContestationPeriod where
  fromInteger i
    | i > 0 = UnsafeContestationPeriod $ fromInteger i
    | otherwise = error $ "ContestationPeriod.fromInteger: " <> toText (show i) <> " is not > 0"

  (+) (UnsafeContestationPeriod a) (UnsafeContestationPeriod b) = UnsafeContestationPeriod (a + b)
  (-) (UnsafeContestationPeriod a) (UnsafeContestationPeriod b) = UnsafeContestationPeriod (a - b)
  (*) (UnsafeContestationPeriod a) (UnsafeContestationPeriod b) = UnsafeContestationPeriod (a * b)
  negate (UnsafeContestationPeriod a) = UnsafeContestationPeriod (negate a)
  abs (UnsafeContestationPeriod a) = UnsafeContestationPeriod (abs a)
  signum (UnsafeContestationPeriod a) = UnsafeContestationPeriod (signum a)

-- | Create a 'ContestationPeriod' from a 'NominalDiffTime'. This will fail if a
-- negative NominalDiffTime is provided and truncates to 1s if values < 1s are given.
fromNominalDiffTime :: MonadFail m => NominalDiffTime -> m ContestationPeriod
fromNominalDiffTime dt =
  if seconds > 0
    then pure . UnsafeContestationPeriod $ ceiling seconds
    else fail $ "fromNominalDiffTime: contestation period <= 0: " <> show dt
 where
  seconds :: Pico = realToFrac dt

toNominalDiffTime :: ContestationPeriod -> NominalDiffTime
toNominalDiffTime (UnsafeContestationPeriod s) =
  secondsToNominalDiffTime $ fromIntegral s
