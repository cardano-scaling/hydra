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
import Test.QuickCheck (oneof)

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
    { queryUTxOByAddress = queryUTxO networkId nodeSocket QueryTip
    , networkId
    }

-- * Local state query

-- | Describes whether to query at the tip or at a specific point.
data QueryPoint = QueryTip | QueryAt ChainPoint
  deriving (Eq, Show, Generic)

instance Arbitrary QueryPoint where
  -- XXX: This is not complete as we lack an 'Arbitrary ChainPoint' and we have
  -- not bothered about it yet.
  arbitrary =
    oneof
      [ pure QueryTip
      , pure $ QueryAt ChainPointAtGenesis
      ]

-- | Query the latest chain point aka "the tip".
queryTip :: NetworkId -> FilePath -> IO ChainPoint
queryTip networkId socket =
  chainTipToChainPoint <$> getLocalChainTip (localNodeConnectInfo networkId socket)

-- | Query the latest chain point just for the slot number.
queryTipSlotNo :: NetworkId -> FilePath -> IO SlotNo
queryTipSlotNo networkId socket =
  getLocalChainTip (localNodeConnectInfo networkId socket) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

-- | Query the system start parameter at given point.
--
-- Throws at least 'QueryException' if query fails.
querySystemStart :: NetworkId -> FilePath -> QueryPoint -> IO SystemStart
querySystemStart networkId socket queryPoint =
  runQuery networkId socket queryPoint QuerySystemStart

-- | Query the era history at given point.
--
-- Throws at least 'QueryException' if query fails.
queryEraHistory :: NetworkId -> FilePath -> QueryPoint -> IO (EraHistory CardanoMode)
queryEraHistory networkId socket queryPoint =
  runQuery networkId socket queryPoint $ QueryEraHistory CardanoModeIsMultiEra

-- | Query the protocol parameters at given point.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters :: NetworkId -> FilePath -> QueryPoint -> IO ProtocolParameters
queryProtocolParameters networkId socket queryPoint =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              QueryProtocolParameters
          )
   in runQuery networkId socket queryPoint query >>= throwOnEraMismatch

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: NetworkId -> FilePath -> QueryPoint -> [Address ShelleyAddr] -> IO UTxO
queryUTxO networkId socket queryPoint addresses =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              ( QueryUTxO
                  (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
              )
          )
   in UTxO.fromApi <$> (runQuery networkId socket queryPoint query >>= throwOnEraMismatch)

-- | Query UTxO for given tx inputs at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn :: NetworkId -> FilePath -> QueryPoint -> [TxIn] -> IO UTxO
queryUTxOByTxIn networkId socket queryPoint inputs =
  let query =
        QueryInEra
          AlonzoEraInCardanoMode
          ( QueryInShelleyBasedEra
              ShelleyBasedEraAlonzo
              (QueryUTxO (QueryUTxOByTxIn (Set.fromList inputs)))
          )
   in UTxO.fromApi <$> (runQuery networkId socket queryPoint query >>= throwOnEraMismatch)

-- | Query the whole UTxO from node at given point. Useful for debugging, but
-- should obviously not be used in production code.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole :: NetworkId -> FilePath -> QueryPoint -> IO UTxO
queryUTxOWhole networkId socket queryPoint = do
  UTxO.fromApi <$> (runQuery networkId socket queryPoint query >>= throwOnEraMismatch)
 where
  query =
    QueryInEra
      AlonzoEraInCardanoMode
      ( QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO QueryUTxOWhole)
      )

-- | Throws at least 'QueryException' if query fails.
runQuery :: NetworkId -> FilePath -> QueryPoint -> QueryInMode CardanoMode a -> IO a
runQuery networkId socket point query =
  queryNodeLocalState (localNodeConnectInfo networkId socket) maybePoint query >>= \case
    Left err -> throwIO $ QueryException (show err)
    Right result -> pure result
 where
  maybePoint =
    case point of
      QueryTip -> Nothing
      QueryAt cp -> Just cp

-- * Helpers

throwOnEraMismatch :: (MonadThrow m) => Either EraMismatch a -> m a
throwOnEraMismatch res =
  case res of
    Left eraMismatch -> throwIO $ QueryException (show eraMismatch)
    Right result -> pure result

localNodeConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams CardanoMode
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64
