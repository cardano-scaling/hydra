-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of
-- cardano-cli's API, using cardano-api types.
module Hydra.Chain.CardanoClient where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Slotting.Time (SystemStart)
import qualified Data.Set as Set
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

type NodeSocket = FilePath

newtype QueryException
  = QueryException Text
  deriving (Eq, Show)

instance Exception QueryException

-- * CardanoClient handle

-- | Handle interface for abstract querying of a cardano node.
data CardanoClient = CardanoClient
  { queryUTxOByAddress :: [Address ShelleyAddr] -> IO UTxO
  , networkId :: NetworkId
  }

-- | Construct a 'CardanoClient' handle.
mkCardanoClient :: NetworkId -> NodeSocket -> CardanoClient
mkCardanoClient networkId nodeSocket =
  CardanoClient
    { queryUTxOByAddress = queryUTxO networkId nodeSocket
    , networkId
    }

-- * Individual functions

queryTip :: NetworkId -> FilePath -> IO ChainPoint
queryTip networkId socket =
  chainTipToChainPoint <$> getLocalChainTip (localNodeConnectInfo networkId socket)

queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket =
  getLocalChainTip (localNodeConnectInfo networkId socket) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

-- | Throws at least 'QueryException' if query fails.
querySystemStart :: NetworkId -> FilePath -> IO SystemStart
querySystemStart networkId socket =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing QuerySystemStart >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result

-- | Throws at least 'QueryException' if query fails.
queryEraHistory :: NetworkId -> FilePath -> IO (EraHistory CardanoMode)
queryEraHistory networkId socket =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing (QueryEraHistory CardanoModeIsMultiEra) >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result

-- | Query current protocol parameters.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters :: NetworkId -> FilePath -> IO ProtocolParameters
queryProtocolParameters networkId socket =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              QueryProtocolParameters
          )
   in runQuery networkId socket query

-- | Query UTxO for all given addresses.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: NetworkId -> FilePath -> [Address ShelleyAddr] -> IO UTxO
queryUTxO networkId socket addresses =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              ( QueryUTxO
                  (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
              )
          )
   in UTxO.fromApi <$> runQuery networkId socket query

-- | Query UTxO for given tx inputs.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn :: NetworkId -> FilePath -> [TxIn] -> IO UTxO
queryUTxOByTxIn networkId socket inputs =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              (QueryUTxO (QueryUTxOByTxIn (Set.fromList inputs)))
          )
   in UTxO.fromApi <$> runQuery networkId socket query

-- | Query the whole UTxO from node. Useful for debugging, but should obviously
-- not be used in production code.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole :: NetworkId -> FilePath -> IO UTxO
queryUTxOWhole networkId socket =
  let query =
        QueryInEra
          BabbageEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraBabbage
              (QueryUTxO QueryUTxOWhole)
          )
   in UTxO.fromApi <$> runQuery networkId socket query

-- | Throws at least 'QueryException' if query fails.
runQuery :: NetworkId -> FilePath -> QueryInMode CardanoMode (Either EraMismatch a) -> IO a
runQuery networkId socket query =
  queryNodeLocalState (localNodeConnectInfo networkId socket) Nothing query >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right (Left eraMismatch) -> throwIO $ QueryException (show eraMismatch)
    Right (Right result) -> pure result

localNodeConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64
