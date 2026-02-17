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
import Data.Set qualified as Set
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

-- * CardanoClient handle

-- | Handle interface for abstract querying of a cardano node.
data CardanoClient era = CardanoClient
  { queryUTxOByAddress :: [Address ShelleyAddr] -> IO (UTxO era)
  , networkId :: NetworkId
  }

-- * Local state query

-- | Describes whether to query at the tip or at a specific point.
data QueryPoint = QueryTip | QueryAt ChainPoint
  deriving stock (Eq, Show, Generic)

-- * CardanoNode Monad

-- | Monad transformer for interacting with a local Cardano node.  Carries
-- both 'LocalNodeConnectInfo' and an era witness so individual query and
-- submission functions need not accept them as explicit arguments.
--
-- 'QueryException' and 'SubmitTransactionException' propagate naturally
-- through the underlying 'MonadThrow' \/ 'MonadCatch' instances.
newtype CardanoNodeT eon era m a = CardanoNodeT
  { fromCardanoNodeT :: ReaderT LocalNodeConnectInfo (EraT eon era m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader LocalNodeConnectInfo
    )

-- | Specialise 'CardanoNodeT' to 'IO'.
type CardanoNode eon era a = CardanoNodeT eon era IO a

-- * EraT Monad Transformer

-- | Monad transformer that carries an era witness.
-- The @eon@ parameter is a higher-kinded era-witness type
-- (e.g. 'ShelleyBasedEra', 'BabbageEraOnwards', 'ConwayEraOnwards') and
-- @era@ is the concrete era (e.g. 'ConwayEra').
newtype EraT eon era m a = EraT
  { fromEraT :: ReaderT (eon era) m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (eon era)
    )

runEraT :: eon era -> EraT eon era m a -> m a
runEraT era = flip runReaderT era . fromEraT

askEra :: Monad m => CardanoNodeT eon era m (eon era)
askEra = CardanoNodeT $ lift ask

-- | Run a 'CardanoNodeIO' action given a connection and era witness.
runCardanoNodeT ::
  LocalNodeConnectInfo ->
  eon era ->
  CardanoNodeT eon era m a ->
  m a
runCardanoNodeT ci era action =
  runEraT era $ runReaderT (fromCardanoNodeT action) ci

runCardanoNode :: LocalNodeConnectInfo -> eon era -> CardanoNode eon era a -> IO a
runCardanoNode = runCardanoNodeT

-- * Tx Construction / Submission

-- | Submit a (signed) transaction to the node.
--
-- Throws 'SubmitTransactionException' if submission fails.
submitTransaction ::
  MonadIO m =>
  Tx ->
  CardanoNodeT ShelleyBasedEra Era m ()
submitTransaction tx = do
  ci <- ask
  sbe <- askEra
  liftIO $
    submitTxToNodeLocal ci (TxInMode sbe tx) >>= \case
      SubmitSuccess -> pure ()
      SubmitFail (TxValidationEraMismatch e) -> throwIO (SubmitEraMismatch e)
      SubmitFail e@TxValidationErrorInCardanoMode{} -> throwIO (SubmitTxValidationError e)

-- | Await until the given transaction is visible on-chain. Returns the UTxO
-- set produced by that transaction.
--
-- Note that this function loops forever; hence, one probably wants to couple it
-- with a surrounding timeout.
awaitTransaction ::
  MonadIO m =>
  Tx ->
  CardanoNodeT ShelleyBasedEra era m (UTxO era)
awaitTransaction tx = go
 where
  ins = keys (UTxO.toMap $ utxoFromTx tx)
  go = do
    utxo <- queryUTxOByTxIn QueryTip ins
    if UTxO.null utxo
      then go
      else pure utxo

-- | Query the latest chain point aka "the tip".
queryTip :: MonadIO m => CardanoNodeT eon era m ChainPoint
queryTip = do
  ci <- ask
  liftIO $ chainTipToChainPoint <$> getLocalChainTip ci

-- | Query the system start parameter at given point.
--
-- Throws at least 'QueryException' if query fails.
querySystemStart :: MonadIO m => QueryPoint -> CardanoNodeT eon era m SystemStart
querySystemStart point =
  runQuery point $
    Query.querySystemStart >>= liftIO . throwOnUnsupportedNtcVersion

-- | Query the era history at given point.
--
-- Throws at least 'QueryException' if query fails.
queryEraHistory :: MonadIO m => QueryPoint -> CardanoNodeT eon era m EraHistory
queryEraHistory point =
  runQuery point $
    Query.queryEraHistory >>= liftIO . throwOnUnsupportedNtcVersion

-- | Query the current epoch number.
--
-- Throws at least 'QueryException' if query fails.
queryEpochNo ::
  MonadIO m =>
  QueryPoint ->
  CardanoNodeT ShelleyBasedEra era m EpochNo
queryEpochNo point = do
  sbe <- askEra
  runQuery point $
    Query.queryEpoch sbe
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query the protocol parameters at given point.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters ::
  MonadIO m =>
  QueryPoint ->
  CardanoNodeT ShelleyBasedEra era m (PParams (ShelleyLedgerEra era))
queryProtocolParameters point = do
  sbe <- askEra
  runQuery point $
    Query.queryProtocolParameters sbe
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query 'GenesisParameters' at a given point.
--
-- Throws at least 'QueryException' if query fails.
queryGenesisParameters ::
  MonadIO m =>
  QueryPoint ->
  CardanoNodeT ShelleyBasedEra era m (GenesisParameters ShelleyEra)
queryGenesisParameters point = do
  sbe <- askEra
  runQuery point $
    Query.queryGenesisParameters sbe
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query UTxO for all given addresses at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxO ::
  MonadIO m =>
  QueryPoint ->
  [Address ShelleyAddr] ->
  CardanoNodeT ShelleyBasedEra era m (UTxO era)
queryUTxO point addresses = do
  sbe <- askEra
  runQuery point $
    Query.queryUtxo sbe (QueryUTxOByAddress (Set.fromList $ map AddressShelley addresses))
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query UTxO for given tx inputs at given point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOByTxIn ::
  MonadIO m =>
  QueryPoint ->
  [TxIn] ->
  CardanoNodeT ShelleyBasedEra era m (UTxO era)
queryUTxOByTxIn point inputs = do
  sbe <- askEra
  runQuery point $
    Query.queryUtxo sbe (QueryUTxOByTxIn (Set.fromList inputs))
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query the whole UTxO from node at given point. Useful for debugging, but
-- should obviously not be used in production code.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOWhole ::
  MonadIO m =>
  QueryPoint ->
  CardanoNodeT ShelleyBasedEra era m (UTxO era)
queryUTxOWhole point = do
  sbe <- askEra
  runQuery point $
    Query.queryUtxo sbe QueryUTxOWhole
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- | Query UTxO for the address of given verification key at point.
--
-- Throws at least 'QueryException' if query fails.
queryUTxOFor ::
  MonadIO m =>
  QueryPoint ->
  VerificationKey PaymentKey ->
  CardanoNodeT ShelleyBasedEra era m (UTxO era)
queryUTxOFor point vk = do
  ci <- ask
  case mkVkAddress (localNodeNetworkId ci) vk of
    ShelleyAddressInEra addr ->
      queryUTxO point [addr]
    ByronAddressInEra{} ->
      error "impossible: mkVkAddress returned Byron address."

-- | Query the current set of registered stake pools.
--
-- Throws at least 'QueryException' if query fails.
queryStakePools ::
  MonadIO m =>
  QueryPoint ->
  CardanoNodeT ShelleyBasedEra era m (Set PoolId)
queryStakePools point = do
  sbe <- askEra
  runQuery point $
    Query.queryStakePools sbe
      >>= liftIO
      . throwOnUnsupportedNtcVersion
      >>= liftIO
      . throwOnEraMismatch

-- * Helpers

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

-- | Execute a local-state query at the given point, using the 'connectInfo'
-- from the reader environment.  Throws 'QueryAcquireException' on failure.
runQuery ::
  MonadIO m =>
  QueryPoint ->
  LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a ->
  CardanoNodeT eon era m a
runQuery point query = do
  ci <- ask
  liftIO $
    executeLocalStateQueryExpr ci target query >>= \case
      Left err -> throwIO $ QueryAcquireException err
      Right result -> pure result
 where
  target =
    case point of
      QueryTip -> VolatileTip
      QueryAt cp -> SpecificPoint cp

-- | Query the current era from the node without needing an era witness.
queryCurrentEraExpr :: LocalStateQueryExpr b p QueryInMode r IO AnyCardanoEra
queryCurrentEraExpr =
  Query.queryCurrentEra >>= liftIO . throwOnUnsupportedNtcVersion

-- | Run a 'CardanoNode' action after dynamically discovering the node's current era.
-- This is useful when connecting to a node whose era is not known in advance.
--
-- Throws 'QueryNotShelleyBasedEraException' if the node is in Byron era.
runNodeIOWithCurrentEra ::
  NetworkId ->
  SocketPath ->
  (forall era. ShelleyBasedEra era -> CardanoNode ShelleyBasedEra era a) ->
  IO a
runNodeIOWithCurrentEra networkId nodeSocket action = do
  let ci = localNodeConnectInfo networkId nodeSocket
  executeLocalStateQueryExpr ci VolatileTip queryCurrentEraExpr >>= \case
    Left err -> throwIO $ QueryAcquireException err
    Right (AnyCardanoEra era) ->
      inEonForEra
        (throwIO $ QueryNotShelleyBasedEraException (anyCardanoEra era))
        (\sbe -> runCardanoNode ci sbe (action sbe))
        era
