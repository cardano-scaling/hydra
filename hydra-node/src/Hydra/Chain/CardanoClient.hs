-- | A basic cardano-node client that can talk to a local cardano-node.
--
-- The idea of this module is to provide a Haskell interface on top of
-- cardano-cli's API, using cardano-api types.
module Hydra.Chain.CardanoClient where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block, UTxO, queryCurrentEra)

import Cardano.Api.Query qualified as Query
import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (eitherDecode', encode)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Printf (printf)

-- XXX: This should be re-exported by cardano-api
-- https://github.com/IntersectMBO/cardano-api/issues/447

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
  { queryUTxOByAddress :: [Address ShelleyAddr] -> IO (UTxO ConwayEra)
  , networkId :: NetworkId
  }

-- * Tx Construction / Submission

-- | Submit a (signed) transaction to the node.
--
-- Throws 'SubmitTransactionException' if submission fails.
submitTransaction ::
  LocalNodeConnectInfo ->
  -- | A signed transaction.
  Tx ->
  IO ()
submitTransaction connectInfo tx =
  submitTxToNodeLocal connectInfo txInMode >>= \case
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
  IsShelleyBasedEra era =>
  LocalNodeConnectInfo ->
  -- | The transaction to watch / await
  Tx ->
  IO (UTxO era)
awaitTransaction connectInfo tx =
  go
 where
  ins = keys (UTxO.toMap $ utxoFromTx tx)
  go = do
    utxo <- queryUTxOByTxIn connectInfo QueryTip ins
    if UTxO.null utxo
      then go
      else pure utxo

-- * Local state query

-- | Describes whether to query at the tip or at a specific point.
data QueryPoint = QueryTip | QueryAt ChainPoint
  deriving stock (Eq, Show, Generic)

-- | Query the latest chain point aka "the tip".
queryTip :: LocalNodeConnectInfo -> IO ChainPoint
queryTip connectInfo =
  chainTipToChainPoint <$> getLocalChainTip connectInfo

-- | Query the system start parameter at given point.
--
-- Throws at least 'QueryException' if query fails.
querySystemStart :: LocalNodeConnectInfo -> QueryPoint -> IO SystemStart
querySystemStart connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $
    Query.querySystemStart >>= liftIO . throwOnUnsupportedNtcVersion

-- | Query the era history at given point.
--
-- Throws at least 'QueryException' if query fails.
queryEraHistory :: LocalNodeConnectInfo -> QueryPoint -> IO EraHistory
queryEraHistory connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $
    Query.queryEraHistory >>= liftIO . throwOnUnsupportedNtcVersion

-- | Query the current epoch number.
--
-- Throws at least 'QueryException' if query fails.
queryEpochNo ::
  LocalNodeConnectInfo ->
  QueryPoint ->
  IO EpochNo
queryEpochNo connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $ do
    queryForCurrentEraInShelleyBasedEraExpr $ \sbe ->
      Query.queryEpoch sbe
        >>= liftIO
        . throwOnUnsupportedNtcVersion
        >>= liftIO
        . throwOnEraMismatch

-- | Query the protocol parameters at given point and convert them to Babbage
-- era protocol parameters.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters ::
  LocalNodeConnectInfo ->
  QueryPoint ->
  IO (PParams LedgerEra)
queryProtocolParameters connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $ do
    queryForCurrentEraInShelleyBasedEraExpr $ \sbe -> do
      eraPParams <-
        Query.queryProtocolParameters sbe
          >>= liftIO
          . throwOnUnsupportedNtcVersion
          >>= liftIO
          . throwOnEraMismatch
      liftIO $ coercePParamsToLedgerEra (convert sbe) eraPParams
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
  LocalNodeConnectInfo ->
  QueryPoint ->
  IO (GenesisParameters ShelleyEra)
queryGenesisParameters connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $ do
    queryForCurrentEraInShelleyBasedEraExpr $ \sbe ->
      Query.queryGenesisParameters sbe
        >>= liftIO
        . throwOnUnsupportedNtcVersion
        >>= liftIO
        . throwOnEraMismatch

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO :: IsShelleyBasedEra era => LocalNodeConnectInfo -> QueryPoint -> [Address ShelleyAddr] -> IO (UTxO era)
queryUTxO connectInfo queryPoint addresses =
  runQueryExpr connectInfo queryPoint $
    Query.queryUtxo shelleyBasedEra (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query UTxO for given tx inputs at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn ::
  IsShelleyBasedEra era =>
  LocalNodeConnectInfo ->
  QueryPoint ->
  [TxIn] ->
  IO (UTxO era)
queryUTxOByTxIn connectInfo queryPoint inputs =
  runQueryExpr connectInfo queryPoint $
    Query.queryUtxo shelleyBasedEra (QueryUTxOByTxIn (Set.fromList inputs))
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

queryForCurrentEraInEonExpr ::
  Eon eon =>
  (AnyCardanoEra -> IO a) ->
  (forall era. eon era -> LocalStateQueryExpr b p QueryInMode r IO a) ->
  LocalStateQueryExpr b p QueryInMode r IO a
queryForCurrentEraInEonExpr no yes = do
  k@(AnyCardanoEra era) <- queryCurrentEraExpr
  inEonForEra (liftIO $ no k) yes era

queryForCurrentEraInShelleyBasedEraExpr ::
  (forall era. ShelleyBasedEra era -> LocalStateQueryExpr b p QueryInMode r IO a) ->
  LocalStateQueryExpr b p QueryInMode r IO a
queryForCurrentEraInShelleyBasedEraExpr = queryForCurrentEraInEonExpr (throwIO . QueryNotShelleyBasedEraException)

-- | Query the whole UTxO from node at given point. Useful for debugging, but
-- should obviously not be used in production code.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole ::
  IsShelleyBasedEra era =>
  LocalNodeConnectInfo ->
  QueryPoint ->
  IO (UTxO era)
queryUTxOWhole connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $
    Query.queryUtxo shelleyBasedEra QueryUTxOWhole
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query UTxO for the address of given verification key at point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOFor :: IsShelleyBasedEra era => LocalNodeConnectInfo -> QueryPoint -> VerificationKey PaymentKey -> IO (UTxO era)
queryUTxOFor connectInfo queryPoint vk =
  case mkVkAddress (localNodeNetworkId connectInfo) vk of
    ShelleyAddressInEra addr ->
      queryUTxO connectInfo queryPoint [addr]
    ByronAddressInEra{} ->
      fail "impossible: mkVkAddress returned Byron address."

-- | Query the current set of registered stake pools.
--
-- Throws at least 'QueryException' if query fails.
queryStakePools ::
  LocalNodeConnectInfo ->
  QueryPoint ->
  IO (Set PoolId)
queryStakePools connectInfo queryPoint =
  runQueryExpr connectInfo queryPoint $ do
    queryForCurrentEraInShelleyBasedEraExpr $ \sbe ->
      Query.queryStakePools sbe
        >>= liftIO
        . throwOnUnsupportedNtcVersion
        >>= liftIO
        . throwOnEraMismatch

-- * Helpers

-- | Monadic query expression to get current era.
queryCurrentEraExpr :: LocalStateQueryExpr b p QueryInMode r IO AnyCardanoEra
queryCurrentEraExpr =
  Query.queryCurrentEra >>= liftIO . throwOnUnsupportedNtcVersion

-- | Throws at least 'QueryException' if query fails.
runQueryExpr ::
  LocalNodeConnectInfo ->
  QueryPoint ->
  LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a ->
  IO a
runQueryExpr connectInfo point query =
  executeLocalStateQueryExpr connectInfo queryTarget query >>= \case
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
