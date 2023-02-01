{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet where

import Hydra.Prelude

import Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo.Data (Data (Data))
import Cardano.Ledger.Alonzo.PlutusScriptApi (language)
import Cardano.Ledger.Alonzo.Scripts (CostModels (CostModels), ExUnits (ExUnits), Tag (Spend), txscriptfee)
import Cardano.Ledger.Alonzo.Tools (TransactionScriptFailure, evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.TxInfo (TranslationError)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (RdmrPtr), Redeemers (..), TxWitness (txrdmrs), txdats, txscripts)
import Cardano.Ledger.Babbage.PParams (PParams, PParams' (..))
import Cardano.Ledger.Babbage.Scripts (refScripts)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..), getLanguageView, hashData, hashScriptIntegrity, referenceInputs)
import Cardano.Ledger.Babbage.TxBody (Datum (..), collateral, inputs, outputs, outputs', scriptIntegrityHash, txfee)
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (HASH, StandardCrypto)
import Cardano.Ledger.Era (ValidateScript (..), fromTxSeq)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Mary.Value (gettriples')
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Serialization (mkSized)
import qualified Cardano.Ledger.Shelley.API as Ledger hiding (TxBody, TxOut)
import Cardano.Ledger.Val (Val (..), invert)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart (..))
import Control.Arrow (left)
import Control.Monad.Class.MonadSTM (
  check,
  newTVarIO,
  readTVarIO,
  writeTVar,
 )
import Data.Array (array)
import qualified Data.List as List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  ChainPoint,
  ConsensusMode (CardanoMode),
  LedgerEra,
  NetworkId,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  VerificationKey,
  fromConsensusPointInMode,
  fromLedgerUTxO,
  makeShelleyAddress,
  shelleyAddressInEra,
  toLedgerAddr,
  verificationKeyHash,
 )
import qualified Hydra.Cardano.Api as Api
import Hydra.Cardano.Api.TxIn (fromLedgerTxIn)
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Chain.Direct.Util (Block, markerDatum)
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)
import Ouroboros.Consensus.Block (blockPoint)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()

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
  { -- | Return all known UTxO addressed to this wallet.
    getUTxO :: STM m (Map TxIn TxOut)
  , -- | Returns the /seed input/
    -- This is the special input needed by `Direct` chain component to initialise
    -- a head
    getSeedInput :: STM m (Maybe Api.TxIn)
  , sign :: ValidatedTx LedgerEra -> ValidatedTx LedgerEra
  , coverFee ::
      Map TxIn TxOut ->
      ValidatedTx LedgerEra ->
      m (Either ErrCoverFee (ValidatedTx LedgerEra))
  , -- | Re-initializ wallet against the latest tip of the node and start to
    -- ignore 'update' calls until reaching that tip.
    reset :: m ()
  , -- | Update the wallet state given some 'Block'. May be ignored if wallet is
    -- still initializing.
    update :: Block -> m ()
  }

data WalletInfoOnChain = WalletInfoOnChain
  { walletUTxO :: Map TxIn TxOut
  , pparams :: PParams LedgerEra
  , systemStart :: SystemStart
  , epochInfo :: EpochInfo (Either Text)
  , -- | Latest point on chain the wallet knows of.
    tip :: ChainPoint
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
      , getSeedInput = fmap (fromLedgerTxIn . fst) . findFuelUTxO <$> getUTxO
      , sign = Util.signWith sk
      , coverFee = \lookupUTxO partialTx -> do
          -- XXX: We should query pparams here. If not, we likely will have
          -- wrong fee estimation should they change in between.
          epochInfo <- queryEpochInfo
          WalletInfoOnChain{walletUTxO, pparams, systemStart} <- readTVarIO walletInfoVar
          pure $ coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx
      , reset = initialize >>= atomically . writeTVar walletInfoVar
      , update = \block -> do
          let point = fromConsensusPointInMode CardanoMode $ blockPoint block
          walletTip <- atomically $ readTVar walletInfoVar <&> \WalletInfoOnChain{tip} -> tip
          if point < walletTip
            then traceWith tracer $ SkipUpdate{point}
            else do
              traceWith tracer $ BeginUpdate{point}
              utxo' <- atomically $ do
                walletInfo@WalletInfoOnChain{walletUTxO} <- readTVar walletInfoVar
                let utxo' = applyBlock block (== ledgerAddress) walletUTxO
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

  ledgerAddress = toLedgerAddr $ shelleyAddressInEra @Api.Era address

-- | Apply a block to our wallet. Does nothing if the transaction does not
-- modify the UTXO set, or else, remove consumed utxos and add produced ones.
--
-- To determine whether a produced output is ours, we apply the given function
-- checking the output's address.
applyBlock :: Block -> (Address -> Bool) -> Map TxIn TxOut -> Map TxIn TxOut
applyBlock blk isOurs utxo = case blk of
  BlockBabbage (ShelleyBlock block _) ->
    flip execState utxo $ do
      forM_ (fromTxSeq $ bbody block) $ \tx -> do
        let txId = getTxId tx
        modify (`Map.withoutKeys` inputs (body tx))
        let indexedOutputs =
              let outs = toList $ outputs' (body tx)
                  maxIx = fromIntegral $ length outs
               in zip [Ledger.TxIx ix | ix <- [0 .. maxIx]] outs
        forM_ indexedOutputs $ \(ix, out@(Ledger.Babbage.TxOut addr _ _ _)) ->
          when (isOurs addr) $ modify (Map.insert (Ledger.TxIn txId ix) out)
  _ ->
    utxo

getTxId ::
  ( HashAlgorithm (HASH crypto)
  , SafeHash.HashAnnotated
      (Ledger.TxBody (era crypto))
      EraIndependentTxBody
      crypto
  ) =>
  ValidatedTx (era crypto) ->
  Ledger.TxId crypto
getTxId tx = Ledger.TxId $ SafeHash.hashAnnotated (body tx)

-- | This are all the error that can happen during coverFee.
data ErrCoverFee
  = ErrNotEnoughFunds ChangeError
  | ErrNoFuelUTxOFound
  | ErrUnknownInput {input :: TxIn}
  | ErrScriptExecutionFailed {scriptFailure :: (RdmrPtr, TransactionScriptFailure StandardCrypto)}
  | ErrTranslationError (TranslationError StandardCrypto)
  deriving (Show)

data ChangeError = ChangeError {inputBalance :: Coin, outputBalance :: Coin}
  deriving (Show)

-- | Cover fee for a transaction body using the given UTXO set. This calculate
-- necessary fees and augments inputs / outputs / collateral accordingly to
-- cover for the transaction cost and get the change back.
--
-- TODO: The fee calculation is currently very dumb and static.
coverFee_ ::
  PParams LedgerEra ->
  SystemStart ->
  EpochInfo (Either Text) ->
  Map TxIn TxOut ->
  Map TxIn TxOut ->
  ValidatedTx LedgerEra ->
  Either ErrCoverFee (ValidatedTx LedgerEra)
coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx@ValidatedTx{body, wits} = do
  (input, output) <- findUTxOToPayFees walletUTxO

  let inputs' = inputs body <> Set.singleton input
  resolvedInputs <- traverse resolveInput (toList inputs')

  let utxo = lookupUTxO <> walletUTxO
  estimatedScriptCosts <-
    estimateScriptsCost pparams systemStart epochInfo utxo partialTx
  let adjustedRedeemers =
        adjustRedeemers
          (inputs body)
          inputs'
          estimatedScriptCosts
          (txrdmrs wits)
      needlesslyHighFee = calculateNeedlesslyHighFee adjustedRedeemers

  change <-
    first ErrNotEnoughFunds $
      mkChange
        output
        resolvedInputs
        (toList $ outputs' body)
        needlesslyHighFee

  let newOutputs = outputs body <> StrictSeq.singleton (mkSized change)
      referenceScripts = refScripts @LedgerEra (referenceInputs body) (Ledger.UTxO utxo)
      langs =
        [ getLanguageView pparams l
        | (_hash, script) <- Map.toList $ Map.union (txscripts wits) referenceScripts
        , (not . isNativeScript @LedgerEra) script
        , l <- maybeToList (language script)
        ]
      finalBody =
        body
          { inputs = inputs'
          , outputs = newOutputs
          , collateral = Set.singleton input
          , txfee = needlesslyHighFee
          , scriptIntegrityHash =
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
  findUTxOToPayFees utxo = case findFuelUTxO utxo of
    Nothing ->
      Left ErrNoFuelUTxOFound
    Just (i, o) ->
      Right (i, o)

  -- TODO: Do a better fee estimation based on the transaction's content.
  calculateNeedlesslyHighFee (Redeemers redeemers) =
    let executionCost = txscriptfee (_prices pparams) $ foldMap snd redeemers
     in Coin 2_000_000 <> executionCost

  getAdaValue :: TxOut -> Coin
  getAdaValue (Ledger.Babbage.TxOut _ value _ _) =
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
  mkChange (Ledger.Babbage.TxOut addr _ datum _) resolvedInputs otherOutputs fee
    -- FIXME: The delta between in and out must be greater than the min utxo value!
    | totalIn <= totalOut =
      Left $
        ChangeError
          { inputBalance = totalIn
          , outputBalance = totalOut
          }
    | otherwise =
      Right $ Ledger.Babbage.TxOut addr (inject changeOut) datum refScript
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
      let ExUnits maxMem maxCpu = _maxTxExUnits pparams
          ExUnits totalMem totalCpu = foldMap identity estimatedCosts
          ExUnits approxMem approxCpu = estimatedCosts ! ptr
       in ExUnits
            (floor (maxMem * approxMem % totalMem))
            (floor (maxCpu * approxCpu % totalCpu))

findFuelUTxO :: Map TxIn TxOut -> Maybe (TxIn, TxOut)
findFuelUTxO utxo =
  let utxosWithDatum = Map.toList $ Map.filter hasMarkerDatum utxo
      sortingCriteria (_, Ledger.Babbage.TxOut _ v _ _) = fst' (gettriples' v)
      sortedByValue = sortOn sortingCriteria utxosWithDatum
   in case sortedByValue of
        [] -> Nothing
        as -> Just (List.last as)
 where
  hasMarkerDatum :: TxOut -> Bool
  hasMarkerDatum (Ledger.Babbage.TxOut _ _ datum _) = case datum of
    NoDatum -> False
    DatumHash dh ->
      dh == hashData (Data @LedgerEra markerDatum)
    Datum{} -> False -- Marker is not stored inline
  fst' (a, _, _) = a

-- | Estimate cost of script executions on the transaction. This is only an
-- estimates because the transaction isn't sealed at this point and adding new
-- elements to it like change outputs or script integrity hash may increase that
-- cost a little.
estimateScriptsCost ::
  -- | Protocol parameters
  PParams LedgerEra ->
  -- | Start of the blockchain, for converting slots to UTC times
  SystemStart ->
  -- | Information about epoch sizes, for converting slots to UTC times
  EpochInfo (Either Text) ->
  -- | A UTXO needed to resolve inputs
  Map TxIn TxOut ->
  -- | The pre-constructed transaction
  ValidatedTx LedgerEra ->
  Either ErrCoverFee (Map RdmrPtr ExUnits)
estimateScriptsCost pparams systemStart epochInfo utxo tx = do
  case result of
    Left translationError ->
      Left $ ErrTranslationError translationError
    Right units ->
      Map.traverseWithKey (\ptr -> left $ ErrScriptExecutionFailed . (ptr,)) units
 where
  result =
    evaluateTransactionExecutionUnits
      pparams
      tx
      (Ledger.UTxO utxo)
      epochInfo
      systemStart
      (costModelsToArray (_costmdls pparams))

  costModelsToArray (CostModels m) =
    array
      (fst (Map.findMin m), fst (Map.findMax m))
      (Map.toList m)

--
-- Logs
--

data TinyWalletLog
  = BeginInitialize
  | EndInitialize {initialUTxO :: Api.UTxO, tip :: ChainPoint}
  | BeginUpdate {point :: ChainPoint}
  | EndUpdate {newUTxO :: Api.UTxO}
  | SkipUpdate {point :: ChainPoint}
  deriving (Eq, Generic, Show)

deriving instance ToJSON TinyWalletLog

instance Arbitrary TinyWalletLog where
  arbitrary = genericArbitrary
