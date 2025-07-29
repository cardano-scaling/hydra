{-# LANGUAGE DuplicateRecordFields #-}

-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsIx (..),
  ExUnits (ExUnits),
  plutusScriptLanguage,
  unAsIx,
 )
import Cardano.Ledger.Alonzo.TxWits (
  Redeemers (..),
  datsTxWitsL,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Api (
  AlonzoEraTx,
  BabbageEraTxBody,
  ConwayEra,
  Data,
  PParams,
  TransactionScriptFailure,
  Tx,
  bodyTxL,
  calcMinFeeTx,
  coinTxOutL,
  collateralInputsTxBodyL,
  ensureMinCoinTxOut,
  evalTxExUnits,
  feeTxBodyL,
  inputsTxBodyL,
  outputsTxBodyL,
  ppMaxTxExUnitsL,
  rdmrsTxWitsL,
  referenceInputsTxBodyL,
  reqSignerHashesTxBodyL,
  scriptIntegrityHashTxBodyL,
  scriptTxWitsL,
  witsTxL,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Api.UTxO (EraUTxO, ScriptsNeeded)
import Cardano.Ledger.Babbage.Tx (body, getLanguageView, hashScriptIntegrity)
import Cardano.Ledger.Babbage.Tx qualified as Babbage
import Cardano.Ledger.Babbage.TxBody qualified as Babbage
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  TxUpgradeError,
 )
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Hashes (EraIndependentTxBody, HashAnnotated, hashAnnotated)
import Cardano.Ledger.Shelley.API (unUTxO)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Val (invert)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart (..))
import Control.Concurrent.Class.MonadSTM (readTVarIO, writeTVar)
import Control.Lens (view, (%~), (.~), (^.))
import Data.List qualified as List
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence.Strict ((|>))
import Data.Set qualified as Set
import Hydra.Cardano.Api (
  BlockHeader,
  ChainPoint,
  LedgerEra,
  NetworkId,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  VerificationKey,
  fromLedgerTx,
  fromLedgerTxIn,
  fromLedgerUTxO,
  getChainPoint,
  makeShelleyAddress,
  shelleyAddressInEra,
  toLedgerAddr,
  toLedgerTx,
  toLedgerUTxO,
  verificationKeyHash,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)

type Address = Ledger.Addr
type TxIn = Ledger.TxIn
type TxOut = Ledger.TxOut LedgerEra

-- | A 'TinyWallet' is a small abstraction of a wallet with basic UTXO
-- management. The wallet is assumed to have only one address, and only one UTXO
-- at that address. It can sign transactions and keeps track of its UTXO behind
-- the scene.
--
-- The wallet is connecting to the node initially and when asked to 'reset'.
-- Otherwise it can be fed blocks via 'update' as the chain rolls forward.
data TinyWallet m = TinyWallet
  { getUTxO :: STM m (Map TxIn TxOut)
  -- ^ Return all known UTxO addressed to this wallet.
  , getSeedInput :: STM m (Maybe Api.TxIn)
  -- ^ Returns the /seed input/
  -- This is the special input needed by `Direct` chain component to initialise
  -- a head
  , sign :: Api.Tx -> Api.Tx
  , coverFee ::
      UTxO ->
      Api.Tx ->
      m (Either ErrCoverFee Api.Tx)
  , reset :: m ()
  -- ^ Re-initializ wallet against the latest tip of the node and start to
  -- ignore 'update' calls until reaching that tip.
  , update :: BlockHeader -> [Api.Tx] -> m ()
  -- ^ Update the wallet state given a block and list of txs. May be ignored if
  -- wallet is still initializing.
  }

data WalletInfoOnChain = WalletInfoOnChain
  { walletUTxO :: Map TxIn TxOut
  , systemStart :: SystemStart
  , tip :: ChainPoint
  -- ^ Latest point on chain the wallet knows of.
  }

type ChainQuery m = QueryPoint -> Api.Address ShelleyAddr -> m WalletInfoOnChain

-- | Create a new tiny wallet handle.
newTinyWallet ::
  -- | A tracer for logging
  Tracer IO TinyWalletLog ->
  -- | Network identifier to generate our address.
  NetworkId ->
  -- | Credentials of the wallet.
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | A function to query UTxO, pparams, system start and epoch info from the
  -- node. Initially and on demand later.
  ChainQuery IO ->
  IO (EpochInfo (Either Text)) ->
  -- | A means to query some pparams.
  IO (PParams ConwayEra) ->
  IO (TinyWallet IO)
newTinyWallet tracer networkId (vk, sk) queryWalletInfo queryEpochInfo querySomePParams = do
  walletInfoVar <- newLabelledTVarIO "tiny-wallet" =<< initialize
  let getUTxO = readTVar walletInfoVar <&> walletUTxO
  pure
    TinyWallet
      { getUTxO
      , getSeedInput = fmap (fromLedgerTxIn . fst) . findLargestUTxO <$> getUTxO
      , sign = Api.signTx sk
      , coverFee = \lookupUTxO partialTx -> do
          let ledgerLookupUTxO = unUTxO $ toLedgerUTxO lookupUTxO
          WalletInfoOnChain{walletUTxO, systemStart} <- readTVarIO walletInfoVar
          epochInfo <- queryEpochInfo
          -- We query pparams here again as it's possible that a hardfork
          -- occurred and the pparams changed.
          pparams <- querySomePParams
          pure $
            fromLedgerTx
              <$> coverFee_ pparams systemStart epochInfo ledgerLookupUTxO walletUTxO (toLedgerTx partialTx)
      , reset = initialize >>= atomically . writeTVar walletInfoVar
      , update = \header txs -> do
          let point = getChainPoint header
          walletTip <- atomically $ readTVar walletInfoVar <&> \WalletInfoOnChain{tip} -> tip
          if point < walletTip
            then traceWith tracer $ SkipUpdate{point}
            else do
              traceWith tracer $ BeginUpdate{point}
              utxo' <- atomically $ do
                walletInfo@WalletInfoOnChain{walletUTxO} <- readTVar walletInfoVar
                let utxo' = applyTxs txs (== ledgerAddress) walletUTxO
                writeTVar walletInfoVar $ walletInfo{walletUTxO = utxo', tip = point}
                pure utxo'
              traceWith tracer $ EndUpdate (fromLedgerUTxO (Ledger.UTxO utxo'))
      }
 where
  initialize = do
    traceWith tracer BeginInitialize
    walletInfo@WalletInfoOnChain{walletUTxO, tip} <- queryWalletInfo QueryTip address
    traceWith tracer $ EndInitialize{initialUTxO = fromLedgerUTxO (Ledger.UTxO walletUTxO), tip}
    pure walletInfo

  address =
    makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress

  ledgerAddress = toLedgerAddr $ shelleyAddressInEra @Api.Era Api.shelleyBasedEra address

-- | Apply a block to our wallet. Does nothing if the transaction does not
-- modify the UTXO set, or else, remove consumed utxos and add produced ones.
--
-- To determine whether a produced output is ours, we apply the given function
-- checking the output's address.
applyTxs :: [Api.Tx] -> (Address -> Bool) -> Map TxIn TxOut -> Map TxIn TxOut
applyTxs txs isOurs utxo =
  flip execState utxo $ do
    forM_ txs $ \apiTx -> do
      -- XXX: Use cardano-api types instead here
      let tx = toLedgerTx apiTx
      let txId = getTxId tx
      modify (`Map.withoutKeys` view inputsTxBodyL (body tx))
      let indexedOutputs =
            let outs = toList $ body tx ^. outputsTxBodyL
                maxIx = fromIntegral $ length outs
             in zip [Ledger.TxIx ix | ix <- [0 .. maxIx]] outs
      forM_ indexedOutputs $ \(ix, out@(Babbage.BabbageTxOut addr _ _ _)) ->
        when (isOurs addr) $ modify (Map.insert (Ledger.TxIn txId ix) out)

getTxId ::
  HashAnnotated
    (Ledger.TxBody era)
    EraIndependentTxBody =>
  Babbage.AlonzoTx era ->
  Ledger.TxId
getTxId tx = Ledger.TxId $ hashAnnotated (body tx)

-- | This are all the error that can happen during coverFee.
data ErrCoverFee
  = ErrNotEnoughFunds ChangeError
  | ErrNoFuelUTxOFound
  | ErrUnknownInput {input :: TxIn}
  | ErrScriptExecutionFailed {redeemerPointer :: Text, scriptFailure :: Text}
  | ErrTranslationError (ContextError LedgerEra)
  | ErrConwayUpgradeError (TxUpgradeError ConwayEra)
  deriving stock (Show)

data ChangeError = ChangeError {inputBalance :: Coin, outputBalance :: Coin}
  deriving stock (Show)

-- | Cover fee for a transaction body using the given UTXO set. This calculate
-- necessary fees and augments inputs / outputs / collateral accordingly to
-- cover for the transaction cost and get the change back.
--
-- XXX: All call sites of this function use cardano-api types
coverFee_ ::
  forall era.
  ( EraPlutusContext era
  , Ledger.EraCertState era
  , AlonzoEraTx era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraUTxO era
  , BabbageEraTxBody era
  ) =>
  PParams era ->
  SystemStart ->
  EpochInfo (Either Text) ->
  Map TxIn (Ledger.TxOut era) ->
  Map TxIn (Ledger.TxOut era) ->
  Tx era ->
  Either ErrCoverFee (Tx era)
coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx = do
  let body = partialTx ^. bodyTxL
  let wits = partialTx ^. witsTxL
  (feeTxIn, feeTxOut) <- findUTxOToPayFees walletUTxO

  let newInputs = body ^. inputsTxBodyL <> Set.singleton feeTxIn
  resolvedInputs <- traverse resolveInput (toList newInputs)

  -- Ensure we have at least the minimum amount of ada. NOTE: setMinCoinTxOut
  -- would invalidate most Hydra protocol transactions.
  let txOuts = body ^. outputsTxBodyL <&> ensureMinCoinTxOut pparams

  -- Compute costs of redeemers
  let utxo = lookupUTxO <> walletUTxO
  estimatedScriptCosts <- estimateScriptsCost pparams systemStart epochInfo utxo partialTx
  let adjustedRedeemers =
        adjustRedeemers
          (body ^. inputsTxBodyL)
          newInputs
          estimatedScriptCosts
          (wits ^. rdmrsTxWitsL)

  -- Compute script integrity hash from adjusted redeemers
  let referenceScripts = getReferenceScripts (Ledger.UTxO utxo) (body ^. referenceInputsTxBodyL)
      langs =
        [ getLanguageView pparams l
        | script <- toList $ (wits ^. scriptTxWitsL) <> referenceScripts
        , l <- maybeToList $ plutusScriptLanguage <$> toPlutusScript script
        ]
      scriptIntegrityHash =
        hashScriptIntegrity
          (Set.fromList langs)
          adjustedRedeemers
          (wits ^. datsTxWitsL)
  let
    unbalancedBody =
      body
        & inputsTxBodyL .~ newInputs
        & outputsTxBodyL .~ txOuts
        & collateralInputsTxBodyL .~ Set.singleton feeTxIn
        & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
    unbalancedTx =
      partialTx
        & bodyTxL .~ unbalancedBody
        & witsTxL . rdmrsTxWitsL .~ adjustedRedeemers

  -- Compute fee using a body with selected txOut to pay fees (= full change)
  -- and an additional witness (we will sign this tx later)
  let fee = calcMinFeeTx (Ledger.UTxO utxo) pparams costingTx additionalWitnesses
      costingTx =
        unbalancedTx
          & bodyTxL . outputsTxBodyL %~ (|> feeTxOut)
          & bodyTxL . feeTxBodyL .~ Coin 10_000_000
      -- We add one additional witness for the fee input
      additionalWitnesses = 1 + length (partialTx ^. bodyTxL . reqSignerHashesTxBodyL)

  -- Balance tx with a change output and computed fee
  change <-
    first ErrNotEnoughFunds $
      mkChange
        feeTxOut
        resolvedInputs
        (toList txOuts)
        fee
  pure $
    unbalancedTx
      & bodyTxL . outputsTxBodyL %~ (|> change)
      & bodyTxL . feeTxBodyL .~ fee
 where
  findUTxOToPayFees :: Map TxIn (Ledger.TxOut era) -> Either ErrCoverFee (TxIn, Ledger.TxOut era)
  findUTxOToPayFees utxo = case findLargestUTxO utxo of
    Nothing ->
      Left ErrNoFuelUTxOFound
    Just (i, o) ->
      Right (i, o)

  resolveInput i = do
    case Map.lookup i (lookupUTxO <> walletUTxO) of
      Nothing -> Left $ ErrUnknownInput i
      Just o -> Right o

  mkChange :: Ledger.TxOut era -> [Ledger.TxOut era] -> [Ledger.TxOut era] -> Coin -> Either ChangeError (Ledger.TxOut era)
  mkChange feeTxOut resolvedInputs otherOutputs fee
    -- FIXME: The delta between in and out must be greater than the min utxo value!
    | totalIn <= totalOut =
        Left $
          ChangeError
            { inputBalance = totalIn
            , outputBalance = totalOut
            }
    | otherwise =
        Right $ feeTxOut & coinTxOutL .~ changeOut
   where
    totalOut = foldMap (view coinTxOutL) otherOutputs <> fee
    totalIn = foldMap (view coinTxOutL) resolvedInputs
    changeOut = totalIn <> invert totalOut

  adjustRedeemers ::
    Set TxIn ->
    Set TxIn ->
    Map (PlutusPurpose AsIx era) ExUnits ->
    Redeemers era ->
    Redeemers era
  adjustRedeemers initialInputs finalInputs estimatedCosts (Redeemers initialRedeemers) =
    Redeemers $ Map.fromList $ map adjustOne $ Map.toList initialRedeemers
   where
    sortedInputs = sort $ toList initialInputs
    sortedFinalInputs = sort $ toList finalInputs
    differences = List.findIndices (not . uncurry (==)) $ zip sortedInputs sortedFinalInputs

    adjustOne :: (PlutusPurpose AsIx era, (Data era, ExUnits)) -> (PlutusPurpose AsIx era, (Data era, ExUnits))
    adjustOne (ptr, (d, _exUnits)) =
      case ptr of
        SpendingPurpose idx
          | fromIntegral (unAsIx idx) `elem` differences ->
              (SpendingPurpose (AsIx (unAsIx idx + 1)), (d, executionUnitsFor ptr))
        _ ->
          (ptr, (d, executionUnitsFor ptr))

    executionUnitsFor :: PlutusPurpose AsIx era -> ExUnits
    executionUnitsFor ptr =
      let ExUnits maxMem maxCpu = pparams ^. ppMaxTxExUnitsL
          ExUnits totalMem totalCpu = foldMap identity estimatedCosts
          ExUnits approxMem approxCpu = estimatedCosts ! ptr
       in ExUnits
            (floor (maxMem * approxMem % totalMem))
            (floor (maxCpu * approxCpu % totalCpu))

findLargestUTxO :: Ledger.EraTxOut era => Map TxIn (Ledger.TxOut era) -> Maybe (TxIn, Ledger.TxOut era)
findLargestUTxO utxo =
  listToMaybe
    . List.sortOn (Down . view coinTxOutL . snd)
    $ Map.toList utxo

-- | Estimate cost of script executions on the transaction. This is only an
-- estimates because the transaction isn't sealed at this point and adding new
-- elements to it like change outputs or script integrity hash may increase that
-- cost a little.
estimateScriptsCost ::
  forall era.
  (AlonzoEraTx era, EraPlutusContext era, ScriptsNeeded era ~ AlonzoScriptsNeeded era, EraUTxO era) =>
  -- | Protocol parameters
  Core.PParams era ->
  -- | Start of the blockchain, for converting slots to UTC times
  SystemStart ->
  -- | Information about epoch sizes, for converting slots to UTC times
  EpochInfo (Either Text) ->
  -- | A UTXO needed to resolve inputs
  Map TxIn (Ledger.TxOut era) ->
  -- | The pre-constructed transaction
  Tx era ->
  Either ErrCoverFee (Map (PlutusPurpose AsIx era) ExUnits)
estimateScriptsCost pparams systemStart epochInfo utxo tx = do
  Map.traverseWithKey convertResult result
 where
  result ::
    Map
      (PlutusPurpose AsIx era)
      (Either (TransactionScriptFailure era) ExUnits)
  result =
    evalTxExUnits
      pparams
      tx
      (Ledger.UTxO utxo)
      epochInfo
      systemStart

  convertResult :: PlutusPurpose AsIx era -> Either (TransactionScriptFailure era) ExUnits -> Either ErrCoverFee ExUnits
  convertResult ptr = \case
    Right exUnits -> Right exUnits
    Left failure ->
      Left $
        ErrScriptExecutionFailed
          { redeemerPointer = show ptr
          , scriptFailure = show failure
          }

--
-- Logs
--

data TinyWalletLog
  = BeginInitialize
  | EndInitialize {initialUTxO :: Api.UTxO, tip :: ChainPoint}
  | BeginUpdate {point :: ChainPoint}
  | EndUpdate {newUTxO :: Api.UTxO}
  | SkipUpdate {point :: ChainPoint}
  deriving stock (Eq, Generic, Show)

deriving anyclass instance ToJSON TinyWalletLog
