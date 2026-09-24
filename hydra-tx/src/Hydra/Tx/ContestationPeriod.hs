module Hydra.Tx.ContestationPeriod where

import Hydra.Prelude hiding (Show, show)

import Data.Fixed (Pico)
import Data.Time (secondsToNominalDiffTime)
import Hydra.Data.ContestationPeriod qualified as OnChain
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

-- | Convert an off-chain contestation period to its on-chain representation.
toChain :: ContestationPeriod -> OnChain.ContestationPeriod
toChain (UnsafeContestationPeriod s) =
  OnChain.UnsafeContestationPeriod
    . fromIntegral
    $ s * 1000

-- | Convert an on-chain contestation period to its off-chain representation,
-- accepting exactly the values 'toChain' can produce: a positive whole number of
-- seconds.
--
-- The on-chain representation is a signed number of milliseconds and nothing
-- constrains it, so an observed datum can carry a value this type cannot hold.
-- Rejecting rather than rounding matters beyond the obvious underflow: the node
-- decides whether to join a head by comparing this value against its own
-- configuration, and then writes 'toChain' of it back into the closing datum,
-- which the head validator requires to equal what the opening datum held. A
-- period that only survives the trip approximately would pass the comparison and
-- then fail that equality forever, leaving a head that can never be closed.
fromChain :: OnChain.ContestationPeriod -> Either Text ContestationPeriod
fromChain cp
  | subSecond == 0 && seconds > 0 = Right . UnsafeContestationPeriod $ fromInteger seconds
  | otherwise =
      Left $
        "contestation period is not a positive whole number of seconds: "
          <> toText (show milliseconds)
          <> "ms"
 where
  milliseconds = toInteger (OnChain.milliseconds cp)

  (seconds, subSecond) = milliseconds `divMod` 1000
