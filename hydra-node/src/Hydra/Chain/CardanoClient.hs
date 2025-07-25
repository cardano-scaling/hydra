-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of
-- cardano-cli's API, using cardano-api types.
module Hydra.Chain.CardanoClient where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block, queryCurrentEra)

import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (eitherDecode', encode)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import Text.Printf (printf)

-- XXX: This should be re-exported by cardano-api
-- https://github.com/IntersectMBO/cardano-api/issues/447

import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

data QueryException
  = QueryAcquireException AcquiringFailure
  | QueryEraMismatchException EraMismatch
  | QueryUnsupportedNtcVersionException UnsupportedNtcVersionError
  | QueryProtocolParamsConversionException ProtocolParametersConversionError
  | QueryProtocolParamsEraNotSupported AnyCardanoEra
  | QueryProtocolParamsEncodingFailureOnEra AnyCardanoEra Text
  | QueryEraNotInCardanoModeFailure AnyCardanoEra
  | QueryNotShelleyBasedEraException AnyCardanoEra
  deriving stock (Show, Eq)

instance Exception QueryException where
  displayException = \case
    QueryAcquireException failure -> show failure
    QueryEraMismatchException EraMismatch{ledgerEraName, otherEraName} ->
      -- NOTE: The "ledger" here is the one in the cardano-node and "otherEra" is the one we picked for the query.
      printf "Connected to cardano-node in unsupported era %s, while we requested %s. Please upgrade your hydra-node." ledgerEraName otherEraName
    QueryUnsupportedNtcVersionException err -> show err
    QueryProtocolParamsConversionException err -> show err
    QueryProtocolParamsEraNotSupported unsupportedEraName ->
      printf "Error while querying protocol params using era %s." (show unsupportedEraName :: Text)
    QueryProtocolParamsEncodingFailureOnEra eraName encodingFailure ->
      printf "Error while querying protocol params using era %s: %s." (show eraName :: Text) encodingFailure
    QueryEraNotInCardanoModeFailure eraName ->
      printf "Error while querying using era %s not in cardano mode." (show eraName :: Text)
    QueryNotShelleyBasedEraException eraName ->
      printf "Error while querying using era %s not in shelley based era." (show eraName :: Text)

-- * CardanoClient handle

-- | Handle interface for abstract querying of a cardano node.
data CardanoClient = CardanoClient
  { queryUTxOByAddress :: [Address ShelleyAddr] -> IO UTxO
  , networkId :: NetworkId
  }

-- | Construct a 'CardanoClient' handle.
mkCardanoClient :: NetworkId -> SocketPath -> CardanoClient
mkCardanoClient networkId nodeSocket =
  CardanoClient
    { queryUTxOByAddress = queryUTxO networkId nodeSocket QueryTip
    , networkId
    }

-- * Tx Construction / Submission

-- | Submit a (signed) transaction to the node.
--
-- Throws 'SubmitTransactionException' if submission fails.
submitTransaction ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  -- | A signed transaction.
  Tx ->
  IO ()
submitTransaction networkId socket tx =
  submitTxToNodeLocal (localNodeConnectInfo networkId socket) txInMode >>= \case
    SubmitSuccess ->
      pure ()
    SubmitFail (TxValidationEraMismatch e) ->
      throwIO (SubmitEraMismatch e)
    SubmitFail e@TxValidationErrorInCardanoMode{} ->
      throwIO (SubmitTxValidationError e)
 where
  txInMode =
    TxInMode shelleyBasedEra tx

-- | Exceptions that 'can' occur during a transaction submission.
--
-- In principle, we can only encounter an 'EraMismatch' at era boundaries, when
-- we try to submit a "next era" transaction as a "current era" transaction, or
-- vice-versa.
-- Similarly, 'TxValidationError' shouldn't occur given that the transaction was
-- safely constructed through 'buildTransaction'.
data SubmitTransactionException
  = SubmitEraMismatch EraMismatch
  | SubmitTxValidationError TxValidationErrorInCardanoMode
  deriving stock (Show)

instance Exception SubmitTransactionException

-- | Await until the given transaction is visible on-chain. Returns the UTxO
-- set produced by that transaction.
--
-- Note that this function loops forever; hence, one probably wants to couple it
-- with a surrounding timeout.
awaitTransaction ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  -- | The transaction to watch / await
  Tx ->
  IO UTxO
awaitTransaction networkId socket tx =
  go
 where
  ins = keys (UTxO.toMap $ utxoFromTx tx)
  go = do
    utxo <- queryUTxOByTxIn networkId socket QueryTip ins
    if UTxO.null utxo
      then go
      else pure utxo

-- * Local state query

-- | Describes whether to query at the tip or at a specific point.
data QueryPoint = QueryTip | QueryAt ChainPoint
  deriving stock (Eq, Show, Generic)

-- | Query the latest chain point aka "the tip".
queryTip :: NetworkId -> SocketPath -> IO ChainPoint
queryTip networkId socket =
  chainTipToChainPoint <$> getLocalChainTip (localNodeConnectInfo networkId socket)

-- | Query the system start parameter at given point.
--
-- Throws at least 'QueryException' if query fails.
querySystemStart :: NetworkId -> SocketPath -> QueryPoint -> IO SystemStart
querySystemStart networkId socket queryPoint =
  runQuery networkId socket queryPoint QuerySystemStart

-- | Query the era history at given point.
--
-- Throws at least 'QueryException' if query fails.
queryEraHistory :: NetworkId -> SocketPath -> QueryPoint -> IO EraHistory
queryEraHistory networkId socket queryPoint =
  runQuery networkId socket queryPoint QueryEraHistory

-- | Query the current epoch number.
--
-- Throws at least 'QueryException' if query fails.
queryEpochNo ::
  NetworkId ->
  SocketPath ->
  QueryPoint ->
  IO EpochNo
queryEpochNo networkId socket queryPoint = do
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    (sbe :: ShelleyBasedEra e) <- liftIO $ assumeShelleyBasedEraOrThrow era
    queryInShelleyBasedEraExpr sbe QueryEpoch

-- | Query the protocol parameters at given point and convert them to Babbage
-- era protocol parameters.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  QueryPoint ->
  IO (PParams LedgerEra)
queryProtocolParameters networkId socket queryPoint =
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    sbe <- liftIO $ assumeShelleyBasedEraOrThrow era
    eraPParams <- queryInShelleyBasedEraExpr sbe QueryProtocolParameters
    liftIO $ coercePParamsToLedgerEra era eraPParams
 where
  encodeToEra :: ToJSON a => CardanoEra era -> a -> IO (PParams LedgerEra)
  encodeToEra eraToEncode pparams =
    case eitherDecode' (encode pparams) of
      Left e -> throwIO $ QueryProtocolParamsEncodingFailureOnEra (anyCardanoEra eraToEncode) (Text.pack e)
      Right (ok :: PParams LedgerEra) -> pure ok

  coercePParamsToLedgerEra :: CardanoEra era -> PParams (ShelleyLedgerEra era) -> IO (PParams LedgerEra)
  coercePParamsToLedgerEra era pparams =
    case era of
      ByronEra -> throwIO $ QueryProtocolParamsEraNotSupported (anyCardanoEra ByronEra)
      ShelleyEra -> encodeToEra ShelleyEra pparams
      AllegraEra -> encodeToEra AllegraEra pparams
      MaryEra -> encodeToEra MaryEra pparams
      AlonzoEra -> encodeToEra AlonzoEra pparams
      BabbageEra -> encodeToEra BabbageEra pparams
      ConwayEra -> pure pparams

-- | Query 'GenesisParameters' at a given point.
--
-- Throws at least 'QueryException' if query fails.
queryGenesisParameters ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  QueryPoint ->
  IO (GenesisParameters ShelleyEra)
queryGenesisParameters networkId socket queryPoint =
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    sbe <- liftIO $ assumeShelleyBasedEraOrThrow era
    queryInShelleyBasedEraExpr sbe QueryGenesisParameters

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: NetworkId -> SocketPath -> QueryPoint -> [Address ShelleyAddr] -> IO UTxO
queryUTxO networkId socket queryPoint addresses =
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    sbe <- liftIO $ assumeShelleyBasedEraOrThrow era
    queryUTxOExpr sbe addresses

queryUTxOExpr :: ShelleyBasedEra era -> [Address ShelleyAddr] -> LocalStateQueryExpr b p QueryInMode r IO UTxO
queryUTxOExpr sbe addresses = do
  eraUTxO <- queryInShelleyBasedEraExpr sbe $ QueryUTxO (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
  pure $ UTxO.fromApi eraUTxO

-- | Query UTxO for given tx inputs at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  QueryPoint ->
  [TxIn] ->
  IO UTxO
queryUTxOByTxIn networkId socket queryPoint inputs =
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    (sbe :: ShelleyBasedEra e) <- liftIO $ assumeShelleyBasedEraOrThrow era
    eraUTxO <- queryInShelleyBasedEraExpr sbe $ QueryUTxO (QueryUTxOByTxIn (Set.fromList inputs))
    pure $ UTxO.fromApi eraUTxO

assumeShelleyBasedEraOrThrow :: MonadThrow m => CardanoEra era -> m (ShelleyBasedEra era)
assumeShelleyBasedEraOrThrow era = do
  x <- requireShelleyBasedEra era
  case x of
    Just sbe -> pure sbe
    Nothing -> throwIO $ QueryNotShelleyBasedEraException (anyCardanoEra era)

-- | Query the whole UTxO from node at given point. Useful for debugging, but
-- should obviously not be used in production code.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  QueryPoint ->
  IO UTxO
queryUTxOWhole networkId socket queryPoint = do
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    (sbe :: ShelleyBasedEra e) <- liftIO $ assumeShelleyBasedEraOrThrow era
    eraUTxO <- queryInShelleyBasedEraExpr sbe $ QueryUTxO QueryUTxOWhole
    pure $ UTxO.fromApi eraUTxO

-- | Query UTxO for the address of given verification key at point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOFor :: NetworkId -> SocketPath -> QueryPoint -> VerificationKey PaymentKey -> IO UTxO
queryUTxOFor networkId nodeSocket queryPoint vk =
  case mkVkAddress networkId vk of
    ShelleyAddressInEra addr ->
      queryUTxO networkId nodeSocket queryPoint [addr]
    ByronAddressInEra{} ->
      fail "impossible: mkVkAddress returned Byron address."

-- | Query the current set of registered stake pools.
--
-- Throws at least 'QueryException' if query fails.
queryStakePools ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  QueryPoint ->
  IO (Set PoolId)
queryStakePools networkId socket queryPoint =
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    (sbe :: ShelleyBasedEra e) <- liftIO $ assumeShelleyBasedEraOrThrow era
    queryInShelleyBasedEraExpr sbe QueryStakePools

-- * Helpers

-- | Monadic query expression to get current era.
queryCurrentEraExpr :: LocalStateQueryExpr b p QueryInMode r IO AnyCardanoEra
queryCurrentEraExpr =
  queryExpr QueryCurrentEra >>= liftIO . throwOnUnsupportedNtcVersion

-- | Monadic query expression for a 'QueryInShelleyBasedEra'.
queryInShelleyBasedEraExpr ::
  -- | The current running era we can use to query the node
  ShelleyBasedEra era ->
  QueryInShelleyBasedEra era a ->
  LocalStateQueryExpr b p QueryInMode r IO a
queryInShelleyBasedEraExpr sbe query =
  queryExpr (QueryInEra $ QueryInShelleyBasedEra sbe query)
    >>= liftIO
    . throwOnUnsupportedNtcVersion
    >>= liftIO
    . throwOnEraMismatch

-- | Throws at least 'QueryException' if query fails.
runQuery :: NetworkId -> SocketPath -> QueryPoint -> QueryInMode a -> IO a
runQuery networkId socket point query =
  runExceptT (queryNodeLocalState (localNodeConnectInfo networkId socket) queryTarget query) >>= \case
    Left err -> throwIO $ QueryAcquireException err
    Right result -> pure result
 where
  queryTarget =
    case point of
      QueryTip -> VolatileTip
      QueryAt cp -> SpecificPoint cp

-- | Throws at least 'QueryException' if query fails.
runQueryExpr ::
  NetworkId ->
  SocketPath ->
  QueryPoint ->
  LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a ->
  IO a
runQueryExpr networkId socket point query =
  executeLocalStateQueryExpr (localNodeConnectInfo networkId socket) queryTarget query >>= \case
    Left err -> throwIO $ QueryAcquireException err
    Right result -> pure result
 where
  queryTarget =
    case point of
      QueryTip -> VolatileTip
      QueryAt cp -> SpecificPoint cp

throwOnEraMismatch :: MonadThrow m => Either EraMismatch a -> m a
throwOnEraMismatch res =
  case res of
    Left eraMismatch -> throwIO $ QueryEraMismatchException eraMismatch
    Right result -> pure result

throwOnUnsupportedNtcVersion :: MonadThrow m => Either UnsupportedNtcVersionError a -> m a
throwOnUnsupportedNtcVersion res =
  case res of
    Left unsupportedNtcVersion -> throwIO $ QueryUnsupportedNtcVersionException unsupportedNtcVersion
    Right result -> pure result

localNodeConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64
