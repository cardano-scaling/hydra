module Hydra.Ledger.Cardano.Configuration (
  module Hydra.Ledger.Cardano.Configuration,
  Ledger.Globals,
  Ledger.LedgerEnv,
) where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Ledger.BaseTypes (Globals (..), boundRational, mkActiveSlotCoeff)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Shelley.API (computeRandomnessStabilisationWindow, computeStabilityWindow)
import Cardano.Ledger.Shelley.API.Types qualified as Ledger
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Data.Aeson qualified as Json
import Data.Aeson.Types qualified as Json

-- * Helpers

readJsonFileThrow :: (Json.Value -> Json.Parser a) -> FilePath -> IO a
readJsonFileThrow parser filepath = do
  value <- Json.eitherDecodeFileStrict filepath >>= either fail pure
  case Json.parseEither parser value of
    Left e -> fail e
    Right a -> pure a

-- * Globals

data GlobalsTranslationException = GlobalsTranslationException
  deriving stock (Eq, Show)

instance Exception GlobalsTranslationException

-- | Create new L2 ledger 'Globals' from 'GenesisParameters'.
--
-- Throws at least 'GlobalsTranslationException'
newGlobals :: MonadThrow m => GenesisParameters ShelleyEra -> m Globals
newGlobals genesisParameters = do
  case mkActiveSlotCoeff <$> boundRational protocolParamActiveSlotsCoefficient of
    Nothing -> throwIO GlobalsTranslationException
    Just slotCoeff -> do
      let k = fromIntegral protocolParamSecurity
      pure $
        Globals
          { activeSlotCoeff = slotCoeff
          , epochInfo
          , maxKESEvo = fromIntegral protocolParamMaxKESEvolutions
          , maxLovelaceSupply = fromIntegral protocolParamMaxLovelaceSupply
          , maxMajorPV
          , networkId = toShelleyNetwork protocolParamNetworkId
          , quorum = fromIntegral protocolParamUpdateQuorum
          , randomnessStabilisationWindow = computeRandomnessStabilisationWindow k slotCoeff
          , securityParameter = k
          , slotsPerKESPeriod = fromIntegral protocolParamSlotsPerKESPeriod
          , stabilityWindow = computeStabilityWindow k slotCoeff
          , systemStart = SystemStart protocolParamSystemStart
          }
 where
  GenesisParameters
    { protocolParamSlotsPerKESPeriod
    , protocolParamUpdateQuorum
    , protocolParamMaxLovelaceSupply
    , protocolParamSecurity
    , protocolParamActiveSlotsCoefficient
    , protocolParamSystemStart
    , protocolParamNetworkId
    , protocolParamMaxKESEvolutions
    , protocolParamEpochLength
    , protocolParamSlotLength
    } = genesisParameters
  -- NOTE: This is used by the ledger to discard blocks that have a version
  -- beyond a known limit. Or said differently, unused and irrelevant for Hydra.
  maxMajorPV = minBound
  -- NOTE: uses fixed epoch info for our L2 ledger
  epochInfo = fixedEpochInfo protocolParamEpochLength slotLength
  slotLength = mkSlotLength protocolParamSlotLength

-- * LedgerEnv

-- | Create a new ledger env from given protocol parameters.
newLedgerEnv :: PParams LedgerEra -> Ledger.LedgerEnv LedgerEra
newLedgerEnv protocolParams =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 0
    , -- NOTE: This can probably stay at 0 forever. This is used internally by the
      -- node's mempool to keep track of transaction seen from peers. Transactions
      -- in Hydra do not go through the node's mempool and follow a different
      -- consensus path so this will remain unused.
      Ledger.ledgerIx = minBound
    , -- NOTE: This keeps track of the ledger's treasury and reserve which are
      -- both unused in Hydra. There might be room for interesting features in the
      -- future with these two but for now, we'll consider them empty.
      Ledger.ledgerAccount = Ledger.AccountState mempty mempty
    , Ledger.ledgerPp = protocolParams
    }
