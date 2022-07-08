module Hydra.Ledger.Cardano.Configuration (
  module Hydra.Ledger.Cardano.Configuration,
  Ledger.Globals,
  Ledger.LedgerEnv,
) where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Control.Arrow (left)
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Ouroboros.Consensus.HardFork.History as Consensus

-- * Helpers

readJsonFileThrow :: (Json.Value -> Json.Parser a) -> FilePath -> IO a
readJsonFileThrow parser filepath = do
  value <- Json.eitherDecodeFileStrict filepath >>= either fail pure
  case Json.parseEither parser value of
    Left e -> fail e
    Right a -> pure a
-- * Globals

shelleyGenesisFromJson :: Json.Value -> Json.Parser (Ledger.ShelleyGenesis LedgerEra)
shelleyGenesisFromJson = parseJSON

newGlobals :: Ledger.ShelleyGenesis LedgerEra -> Ledger.Globals
newGlobals shelleyGenesis =
  Ledger.mkShelleyGlobals
    shelleyGenesis
    epochInfo
    maxProtocolVersion
 where
  -- NOTE: This is used by the ledger to discard blocks that have a version
  -- beyond a known limit. Or said differently, unused and irrelevant for Hydra.
  maxProtocolVersion = 0
  epochInfo =
    Consensus.neverForksSummary epochSize slotLength
      & Consensus.summaryToEpochInfo
      & hoistEpochInfo (left show . runExcept)
  epochSize = Ledger.sgEpochLength shelleyGenesis
  slotLength = mkSlotLength (Ledger.sgSlotLength shelleyGenesis)
-- * LedgerEnv

protocolParametersFromJson :: Json.Value -> Json.Parser ProtocolParameters
protocolParametersFromJson = parseJSON

newLedgerEnv :: ProtocolParameters -> Ledger.LedgerEnv LedgerEra
newLedgerEnv pparams =
  Ledger.LedgerEnv
    { -- TODO: Ideally we would want the slot number to be initialized to whatever
      -- the slot number is on the underlying mainchain. This is somewhat hard to
      -- figure out however because we don't really have a formal definition of
      -- when a head start? (is it when the first initTx is posted? Is is after
      -- the collectCom?). That definition needs to be ubiquitous for all parties
      -- and deterministically computable from on-chain observation AND preferably
      -- in sync with mainnet time.
      --
      -- Another issue also is that our ledger in a head does not advance time.
      -- So, the slot number will at the moment stay at 0 and any script relying
      -- on time progression will not work correctly.
      Ledger.ledgerSlotNo = SlotNo 0
    , -- NOTE: This can probably stay at 0 forever. This is used internally by the
      -- node's mempool to keep track of transaction seen from peers. Transactions
      -- in Hydra do not go through the node's mempool and follow a different
      -- consensus path so this will remain unused.
      Ledger.ledgerIx = minBound
    , -- NOTE: This keeps track of the ledger's treasury and reserve which are
      -- both unused in Hydra. There might be room for interesting features in the
      -- future with these two but for now, we'll consider them empty.
      Ledger.ledgerAccount = Ledger.AccountState mempty mempty
    , Ledger.ledgerPp = toLedgerPParams ShelleyBasedEraBabbage pparams
    }
