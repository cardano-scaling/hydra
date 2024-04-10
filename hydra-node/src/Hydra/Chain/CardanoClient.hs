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
import Test.QuickCheck (oneof)
import Text.Printf (printf)

-- XXX: This should be re-exported by cardano-api
-- https://github.com/IntersectMBO/cardano-api/issues/447

import Cardano.Ledger.Core qualified as Ledger
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

data QueryException
  = QueryAcquireException AcquiringFailure
  | QueryEraMismatchException EraMismatch
  | QueryProtocolParamsConversionException ProtocolParametersConversionError
  | QueryProtocolParamsEraNotSupported AnyCardanoEra
  | QueryProtocolParamsEncodingFailureOnEra AnyCardanoEra Text
  | QueryEraNotInCardanoModeFailure AnyCardanoEra
  | QueryNotShelleyBasedEraException AnyCardanoEra
  deriving stock (Show)

instance Eq QueryException where
  a == b = case (a, b) of
    (QueryAcquireException af1, QueryAcquireException af2) -> case (af1, af2) of
      (AFPointTooOld, AFPointTooOld) -> True
      (AFPointNotOnChain, AFPointNotOnChain) -> True
      _ -> False
    (QueryEraMismatchException em1, QueryEraMismatchException em2) -> em1 == em2
    (QueryProtocolParamsEraNotSupported ens1, QueryProtocolParamsEraNotSupported ens2) -> ens1 == ens2
    (QueryProtocolParamsEncodingFailureOnEra e1 f1, QueryProtocolParamsEncodingFailureOnEra e2 f2) -> e1 == e2 && f1 == f2
    (QueryEraNotInCardanoModeFailure e1, QueryEraNotInCardanoModeFailure e2) -> e1 == e2
    (QueryNotShelleyBasedEraException e1, QueryNotShelleyBasedEraException e2) -> e1 == e2
    _ -> False

instance Exception QueryException where
  displayException = \case
    QueryAcquireException failure -> show failure
    QueryEraMismatchException EraMismatch{ledgerEraName, otherEraName} ->
      printf "Connected to cardano-node in unsupported era %s. Please upgrade your hydra-node to era %s." otherEraName ledgerEraName
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

-- | Construct a simple payment consuming some inputs and producing some
-- outputs (no certificates or withdrawals involved).
--
-- On success, the returned transaction is fully balanced. On error, return
-- `TxBodyErrorAutoBalance`.
buildTransaction ::
  -- | Current network identifier
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  -- | Change address to send
  AddressInEra ->
  -- | Unspent transaction outputs to spend.
  UTxO ->
  -- | Collateral inputs.
  [TxIn] ->
  -- | Outputs to create.
  [TxOut CtxTx] ->
  IO (Either (TxBodyErrorAutoBalance Era) TxBody)
buildTransaction networkId socket changeAddress utxoToSpend collateral outs = do
  pparams <- queryProtocolParameters networkId socket QueryTip
  systemStart <- querySystemStart networkId socket QueryTip
  eraHistory <- queryEraHistory networkId socket QueryTip
  stakePools <- queryStakePools networkId socket QueryTip
  pure $
    second balancedTxBody $
      makeTransactionBodyAutoBalance
        shelleyBasedEra
        systemStart
        (toLedgerEpochInfo eraHistory)
        (LedgerProtocolParameters pparams)
        stakePools
        mempty
        mempty
        (UTxO.toApi utxoToSpend)
        (bodyContent pparams)
        changeAddress
        Nothing
 where
  -- NOTE: 'makeTransactionBodyAutoBalance' overwrites this.
  dummyFeeForBalancing = TxFeeExplicit 0

  bodyContent pparams =
    TxBodyContent
      (withWitness <$> toList (UTxO.inputSet utxoToSpend))
      (TxInsCollateral collateral)
      TxInsReferenceNone
      outs
      TxTotalCollateralNone
      TxReturnCollateralNone
      dummyFeeForBalancing
      TxValidityNoLowerBound
      TxValidityNoUpperBound
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith $ Just $ LedgerProtocolParameters pparams)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintValueNone
      TxScriptValidityNone
      Nothing
      Nothing

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
    if null utxo
      then go
      else pure utxo

-- * Local state query

-- | Describes whether to query at the tip or at a specific point.
data QueryPoint = QueryTip | QueryAt ChainPoint
  deriving stock (Eq, Show, Generic)

deriving anyclass instance ToJSON QueryPoint

instance Arbitrary QueryPoint where
  -- XXX: This is not complete as we lack an 'Arbitrary ChainPoint' and we have
  -- not bothered about it yet.
  arbitrary =
    oneof
      [ pure QueryTip
      , pure $ QueryAt ChainPointAtGenesis
      ]

-- | Query the latest chain point aka "the tip".
queryTip :: NetworkId -> SocketPath -> IO ChainPoint
queryTip networkId socket =
  chainTipToChainPoint <$> getLocalChainTip (localNodeConnectInfo networkId socket)

-- | Query the latest chain point just for the slot number.
queryTipSlotNo :: NetworkId -> SocketPath -> IO SlotNo
queryTipSlotNo networkId socket =
  getLocalChainTip (localNodeConnectInfo networkId socket) >>= \case
    ChainTipAtGenesis -> pure 0
    ChainTip slotNo _ _ -> pure slotNo

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

-- | Query the protocol parameters at given point.
--
-- Throws at least 'QueryException' if query fails.
queryProtocolParameters ::
  -- | Current network discriminant
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  QueryPoint ->
  IO PParams
queryProtocolParameters networkId socket queryPoint =
  runQueryExpr networkId socket queryPoint $ do
    (AnyCardanoEra era) <- queryCurrentEraExpr
    sbe <- liftIO $ assumeShelleyBasedEraOrThrow era
    eraPParams <- queryInShelleyBasedEraExpr sbe QueryProtocolParameters
    liftIO $ coercePParamsToLedgerEra era eraPParams
 where
  encodeToEra eraToEncode pparams =
    case eitherDecode' (encode pparams) of
      Left e -> throwIO $ QueryProtocolParamsEncodingFailureOnEra (anyCardanoEra eraToEncode) (Text.pack e)
      Right ok -> pure ok

  coercePParamsToLedgerEra :: CardanoEra era -> Ledger.PParams (ShelleyLedgerEra era) -> IO PParams
  coercePParamsToLedgerEra era pparams =
    case era of
      ByronEra -> throwIO $ QueryProtocolParamsEraNotSupported (anyCardanoEra ByronEra)
      ShelleyEra -> encodeToEra ShelleyEra pparams
      AllegraEra -> encodeToEra AllegraEra pparams
      MaryEra -> encodeToEra MaryEra pparams
      AlonzoEra -> encodeToEra AlonzoEra pparams
      BabbageEra -> pure pparams
      ConwayEra -> encodeToEra ConwayEra pparams

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
  queryNodeLocalState (localNodeConnectInfo networkId socket) queryTarget query >>= \case
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
    Left unsupportedNtcVersion -> error $ show unsupportedNtcVersion -- TODO
    Right result -> pure result

localNodeConnectInfo :: NetworkId -> SocketPath -> LocalNodeConnectInfo
localNodeConnectInfo = LocalNodeConnectInfo cardanoModeParams

cardanoModeParams :: ConsensusModeParams
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
 where
  -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
  -- is the default for cardano-cli
  defaultByronEpochSlots = 21600 :: Word64
