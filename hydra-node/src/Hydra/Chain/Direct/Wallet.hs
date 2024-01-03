{-# LANGUAGE DuplicateRecordFields #-}

-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TranslationError)
import Cardano.Ledger.Alonzo.PlutusScriptApi (language)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (ExUnits), Tag (Spend), txscriptfee)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), RdmrPtr (RdmrPtr), Redeemers (..), txdats, txscripts)
import Cardano.Ledger.Api (TransactionScriptFailure, ensureMinCoinTxOut, evalTxExUnits, outputsTxBodyL, ppMaxTxExUnitsL, ppPricesL)
import Cardano.Ledger.Babbage.Tx (body, getLanguageView, hashScriptIntegrity, wits)
import Cardano.Ledger.Babbage.Tx qualified as Babbage
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), spendInputs')
import Cardano.Ledger.Babbage.TxBody qualified as Babbage
import Cardano.Ledger.Babbage.UTxO (getReferenceScripts)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (isNativeScript)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto (HASH, StandardCrypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.SafeHash qualified as SafeHash
import Cardano.Ledger.Shelley.API (unUTxO)
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Val (Val (..), invert)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart (..))
import Control.Arrow (left)
import Control.Concurrent.Class.MonadSTM (check, newTVarIO, readTVarIO, writeTVar)
import Control.Lens ((^.))
import Data.List qualified as List
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ratio ((%))
import Data.Sequence.Strict qualified as StrictSeq
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
  fromLedgerTxOut,
  fromLedgerUTxO,
  getChainPoint,
  ledgerEraVersion,
  makeShelleyAddress,
  selectLovelace,
  shelleyAddressInEra,
  toLedgerAddr,
  toLedgerTx,
  toLedgerTxIn,
  toLedgerTxOut,
  toLedgerUTxO,
  verificationKeyHash,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)

type Address = Ledger.Addr StandardCrypto
type TxIn = Ledger.TxIn StandardCrypto
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
  , pparams :: Core.PParams LedgerEra
  , systemStart :: SystemStart
  , epochInfo :: EpochInfo (Either Text)
  , tip :: ChainPoint
  -- ^ Latest point on chain the wallet knows of.
  }

type ChainQuery m = QueryPoint -> Api.Address ShelleyAddr -> m WalletInfoOnChain

watchUTxOUntil :: (Map TxIn TxOut -> Bool) -> TinyWallet IO -> IO (Map TxIn TxOut)
watchUTxOUntil predicate TinyWallet{getUTxO} = atomically $ do
  u <- getUTxO
  u <$ check (predicate u)

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
  IO (TinyWallet IO)
newTinyWallet tracer networkId (vk, sk) queryWalletInfo queryEpochInfo = do
  walletInfoVar <- newTVarIO =<< initialize
  let getUTxO = readTVar walletInfoVar <&> walletUTxO
  pure
    TinyWallet
      { getUTxO
      , getSeedInput = fmap (fromLedgerTxIn . fst) . findLargestUTxO <$> getUTxO
      , sign = Api.signTx sk
      , coverFee = \lookupUTxO partialTx -> do
          -- XXX: We should query pparams here. If not, we likely will have
          -- wrong fee estimation should they change in between.
          epochInfo <- queryEpochInfo
          WalletInfoOnChain{walletUTxO, pparams, systemStart} <- readTVarIO walletInfoVar
          pure $
            fromLedgerTx
              <$> coverFee_ pparams systemStart epochInfo (unUTxO $ toLedgerUTxO lookupUTxO) walletUTxO (toLedgerTx partialTx)
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
      modify (`Map.withoutKeys` spendInputs' (body tx))
      let indexedOutputs =
            let outs = toList $ body tx ^. outputsTxBodyL
                maxIx = fromIntegral $ length outs
             in zip [Ledger.TxIx ix | ix <- [0 .. maxIx]] outs
      forM_ indexedOutputs $ \(ix, out@(Babbage.BabbageTxOut addr _ _ _)) ->
        when (isOurs addr) $ modify (Map.insert (Ledger.TxIn txId ix) out)

getTxId ::
  ( HashAlgorithm (HASH crypto)
  , SafeHash.HashAnnotated
      (Ledger.TxBody (era crypto))
      EraIndependentTxBody
      crypto
  ) =>
  Babbage.AlonzoTx (era crypto) ->
  Ledger.TxId crypto
getTxId tx = Ledger.TxId $ SafeHash.hashAnnotated (body tx)

-- | This are all the error that can happen during coverFee.
data ErrCoverFee
  = ErrNotEnoughFunds ChangeError
  | ErrNoFuelUTxOFound
  | ErrUnknownInput {input :: TxIn}
  | ErrScriptExecutionFailed {scriptFailure :: (RdmrPtr, TransactionScriptFailure LedgerEra)}
  | ErrTranslationError (TranslationError StandardCrypto)
  deriving stock (Show)

data ChangeError = ChangeError {inputBalance :: Coin, outputBalance :: Coin}
  deriving stock (Show)

-- | Cover fee for a transaction body using the given UTXO set. This calculate
-- necessary fees and augments inputs / outputs / collateral accordingly to
-- cover for the transaction cost and get the change back.
--
-- TODO: The fee calculation is currently very dumb and static.
-- XXX: All call sites of this function use cardano-api types
coverFee_ ::
  Core.PParams LedgerEra ->
  SystemStart ->
  EpochInfo (Either Text) ->
  Map TxIn TxOut ->
  Map TxIn TxOut ->
  Babbage.AlonzoTx LedgerEra ->
  Either ErrCoverFee (Babbage.AlonzoTx LedgerEra)
coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx@Babbage.AlonzoTx{body, wits} = do
  (input, output) <- findUTxOToPayFees walletUTxO

  let newInputs = spendInputs' body <> Set.singleton input
  resolvedInputs <- traverse resolveInput (toList newInputs)

  let utxo = lookupUTxO <> walletUTxO
  estimatedScriptCosts <-
    estimateScriptsCost pparams systemStart epochInfo utxo partialTx
  let adjustedRedeemers =
        adjustRedeemers
          (spendInputs' body)
          newInputs
          estimatedScriptCosts
          (txrdmrs wits)
      needlesslyHighFee = calculateNeedlesslyHighFee adjustedRedeemers

  -- Ensure we have at least the minimum amount of ada. NOTE: setMinCointTxOut
  -- would invalidate most Hydra protocol transactions.
  let txOuts = body ^. outputsTxBodyL <&> ensureMinCoinTxOut pparams

  -- Add a change output
  change <-
    first ErrNotEnoughFunds $
      mkChange
        output
        resolvedInputs
        (toList txOuts)
        needlesslyHighFee
  let newOutputs = txOuts <> StrictSeq.singleton change

      referenceScripts = getReferenceScripts @LedgerEra (Ledger.UTxO utxo) (Babbage.referenceInputs' body)
      langs =
        [ getLanguageView pparams l
        | (_hash, script) <- Map.toList $ Map.union (txscripts wits) referenceScripts
        , (not . isNativeScript @LedgerEra) script
        , l <- maybeToList (language script)
        ]
      finalBody =
        body
          { btbInputs = newInputs
          , btbOutputs = mkSized ledgerEraVersion <$> newOutputs
          , btbCollateral = Set.singleton input
          , btbTxFee = needlesslyHighFee
          , btbScriptIntegrityHash =
              hashScriptIntegrity
                (Set.fromList langs)
                adjustedRedeemers
                (txdats wits)
          }
  pure $
    partialTx
      { body = finalBody
      , wits = wits{txrdmrs = adjustedRedeemers}
      }
 where
  findUTxOToPayFees utxo = case findLargestUTxO utxo of
    Nothing ->
      Left ErrNoFuelUTxOFound
    Just (i, o) ->
      Right (i, o)

  -- TODO: Do a better fee estimation based on the transaction's content.
  calculateNeedlesslyHighFee (Redeemers redeemers) =
    let executionCost = txscriptfee (pparams ^. ppPricesL) $ foldMap snd redeemers
     in Coin 2_000_000 <> executionCost

  getAdaValue :: TxOut -> Coin
  getAdaValue (Babbage.BabbageTxOut _ value _ _) =
    coin value

  resolveInput :: TxIn -> Either ErrCoverFee TxOut
  resolveInput i = do
    case Map.lookup i (lookupUTxO <> walletUTxO) of
      Nothing -> Left $ ErrUnknownInput i
      Just o -> Right o

  mkChange ::
    TxOut ->
    [TxOut] ->
    [TxOut] ->
    Coin ->
    Either ChangeError TxOut
  mkChange (Babbage.BabbageTxOut addr _ datum _) resolvedInputs otherOutputs fee
    -- FIXME: The delta between in and out must be greater than the min utxo value!
    | totalIn <= totalOut =
        Left $
          ChangeError
            { inputBalance = totalIn
            , outputBalance = totalOut
            }
    | otherwise =
        Right $ Babbage.BabbageTxOut addr (inject changeOut) datum refScript
   where
    totalOut = foldMap getAdaValue otherOutputs <> fee
    totalIn = foldMap getAdaValue resolvedInputs
    changeOut = totalIn <> invert totalOut
    refScript = SNothing

  adjustRedeemers :: Set TxIn -> Set TxIn -> Map RdmrPtr ExUnits -> Redeemers LedgerEra -> Redeemers LedgerEra
  adjustRedeemers initialInputs finalInputs estimatedCosts (Redeemers initialRedeemers) =
    Redeemers $ Map.fromList $ map adjustOne $ Map.toList initialRedeemers
   where
    sortedInputs = sort $ toList initialInputs
    sortedFinalInputs = sort $ toList finalInputs
    differences = List.findIndices (not . uncurry (==)) $ zip sortedInputs sortedFinalInputs

    adjustOne (ptr, (d, _exUnits)) =
      case ptr of
        RdmrPtr Spend idx
          | fromIntegral idx `elem` differences ->
              (RdmrPtr Spend (idx + 1), (d, executionUnitsFor ptr))
        _ ->
          (ptr, (d, executionUnitsFor ptr))

    executionUnitsFor :: RdmrPtr -> ExUnits
    executionUnitsFor ptr =
      let ExUnits maxMem maxCpu = pparams ^. ppMaxTxExUnitsL
          ExUnits totalMem totalCpu = foldMap identity estimatedCosts
          ExUnits approxMem approxCpu = estimatedCosts ! ptr
       in ExUnits
            (floor (maxMem * approxMem % totalMem))
            (floor (maxCpu * approxCpu % totalCpu))

findLargestUTxO :: Map TxIn TxOut -> Maybe (TxIn, TxOut)
findLargestUTxO utxo =
  maxLovelaceUTxO apiUtxo
 where
  apiUtxo = UTxO.fromPairs $ bimap fromLedgerTxIn fromLedgerTxOut <$> Map.toList utxo

  maxLovelaceUTxO =
    fmap (bimap toLedgerTxIn toLedgerTxOut)
      . listToMaybe
      . List.sortOn (Down . selectLovelace . Api.txOutValue . snd)
      . UTxO.pairs

-- | Estimate cost of script executions on the transaction. This is only an
-- estimates because the transaction isn't sealed at this point and adding new
-- elements to it like change outputs or script integrity hash may increase that
-- cost a little.
estimateScriptsCost ::
  -- | Protocol parameters
  Core.PParams LedgerEra ->
  -- | Start of the blockchain, for converting slots to UTC times
  SystemStart ->
  -- | Information about epoch sizes, for converting slots to UTC times
  EpochInfo (Either Text) ->
  -- | A UTXO needed to resolve inputs
  Map TxIn TxOut ->
  -- | The pre-constructed transaction
  Babbage.AlonzoTx LedgerEra ->
  Either ErrCoverFee (Map RdmrPtr ExUnits)
estimateScriptsCost pparams systemStart epochInfo utxo tx = do
  case result of
    Left translationError ->
      Left $ ErrTranslationError translationError
    Right units ->
      Map.traverseWithKey (\ptr -> left $ ErrScriptExecutionFailed . (ptr,)) units
 where
  result =
    evalTxExUnits
      pparams
      tx
      (Ledger.UTxO utxo)
      epochInfo
      systemStart

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
deriving anyclass instance FromJSON TinyWalletLog

instance Arbitrary TinyWalletLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
