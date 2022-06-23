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
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..), hashData, hashScriptIntegrity)
import Cardano.Ledger.Babbage.TxBody (Datum (..), collateral, inputs, outputs, scriptIntegrityHash, txfee)
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (HASH, StandardCrypto)
import Cardano.Ledger.Era (ValidateScript (..), fromTxSeq)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger hiding (TxBody, TxOut)
import Cardano.Ledger.Val (Val (..), invert)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart (..))
import Control.Arrow (left)
import Control.Exception (throw)
import Control.Monad.Class.MonadSTM (
  check,
  newTVarIO,
  writeTVar,
 )
import Data.Aeson (Value (String), object, (.=))
import Data.Array (array)
import qualified Data.List as List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  LedgerEra,
  NetworkId,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  ShelleyAddr,
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  VerificationKey,
  makeShelleyAddress,
  shelleyAddressInEra,
  toLedgerAddr,
  verificationKeyHash,
 )
import qualified Hydra.Cardano.Api as Api
import Hydra.Chain.CardanoClient (QueryPoint (QueryTip))
import Hydra.Chain.Direct.Util (Block, markerDatum)
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Logging (Tracer, traceWith)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockBabbage))
import Ouroboros.Consensus.HardFork.History (PastHorizonException)
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
-- This wallet is not connecting to the node initially and when asked to
-- 'reset'. Otherwise it can be fed blocks via 'update' as the chain rolls
-- forward.
data TinyWallet m = TinyWallet
  { -- | Return all known UTxO addressed to this wallet.
    getUTxO :: STM m (Map TxIn TxOut)
  , sign :: ValidatedTx LedgerEra -> ValidatedTx LedgerEra
  , coverFee :: Map TxIn TxOut -> ValidatedTx LedgerEra -> STM m (Either ErrCoverFee (ValidatedTx LedgerEra))
  , -- | Reset the wallet state to some point.
    reset :: QueryPoint -> m ()
  , -- | Update the wallet state given some 'Block'.
    update :: Block -> m ()
  }

type ChainQuery m =
  ( QueryPoint ->
    Api.Address ShelleyAddr ->
    m
      ( Map TxIn TxOut
      , PParams LedgerEra
      , SystemStart
      , EpochInfo (Except PastHorizonException)
      )
  )

-- | Get a single, marked as "fuel" UTxO.
getFuelUTxO :: MonadSTM m => TinyWallet m -> STM m (Maybe (TxIn, TxOut))
getFuelUTxO TinyWallet{getUTxO} =
  findFuelUTxO <$> getUTxO

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
  IO (TinyWallet IO)
newTinyWallet tracer networkId (vk, sk) queryUTxOEtc = do
  utxoVar <- newTVarIO =<< queryUTxOEtc QueryTip address
  pure
    TinyWallet
      { getUTxO =
          (\(u, _, _, _) -> u) <$> readTVar utxoVar
      , sign = Util.signWith (vk, sk)
      , coverFee = \lookupUTxO partialTx -> do
          (walletUTxO, pparams, systemStart, epochInfo) <- readTVar utxoVar
          case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx of
            Left e ->
              pure (Left e)
            Right (walletUTxO', balancedTx) -> do
              writeTVar utxoVar (walletUTxO', pparams, systemStart, epochInfo)
              pure (Right balancedTx)
      , reset = \point -> do
          res@(u, _, _, _) <- queryUTxOEtc point address
          atomically $ writeTVar utxoVar res
          traceWith tracer $ InitializingWallet point u
      , update = \block -> do
          utxo' <- atomically $ do
            (utxo, pparams, systemStart, epochInfo) <- readTVar utxoVar
            let utxo' = applyBlock block (== ledgerAddress) utxo
            writeTVar utxoVar (utxo', pparams, systemStart, epochInfo)
            pure utxo'
          traceWith tracer $ ApplyBlock utxo'
      }
 where
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
              let outs = outputs (body tx)
                  maxIx = fromIntegral $ length outs
               in StrictSeq.zip (StrictSeq.fromList [Ledger.TxIx ix | ix <- [0 .. maxIx]]) outs
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

data ErrCoverFee
  = ErrNoAvailableUTxO
  | ErrNotEnoughFunds ChangeError
  | ErrUnknownInput {input :: TxIn}
  | ErrNoPaymentUTxOFound
  | ErrScriptExecutionFailed (RdmrPtr, TransactionScriptFailure StandardCrypto)
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
  EpochInfo (Except PastHorizonException) ->
  Map TxIn TxOut ->
  Map TxIn TxOut ->
  ValidatedTx LedgerEra ->
  Either ErrCoverFee (Map TxIn TxOut, ValidatedTx LedgerEra)
coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx@ValidatedTx{body, wits} = do
  (input, output) <- findUTxOToPayFees walletUTxO

  let inputs' = inputs body <> Set.singleton input
  resolvedInputs <- traverse resolveInput (toList inputs')

  estimatedScriptCosts <-
    left ErrScriptExecutionFailed $
      estimateScriptsCost pparams systemStart epochInfo (lookupUTxO <> walletUTxO) partialTx
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
        (toList $ outputs body)
        needlesslyHighFee

  let outputs' = outputs body <> StrictSeq.singleton change
      langs =
        [ l
        | (_hash, script) <- Map.toList (txscripts wits)
        , (not . isNativeScript @LedgerEra) script
        , Just l <- [language script]
        ]
      finalBody =
        body
          { inputs = inputs'
          , outputs = outputs'
          , collateral = Set.singleton input
          , txfee = needlesslyHighFee
          , scriptIntegrityHash =
              hashScriptIntegrity
                pparams
                (Set.fromList langs)
                adjustedRedeemers
                (txdats wits)
          }
  pure
    ( Map.withoutKeys walletUTxO inputs'
    , partialTx
        { body = finalBody
        , wits = wits{txrdmrs = adjustedRedeemers}
        }
    )
 where
  findUTxOToPayFees utxo = case findFuelUTxO utxo of
    Nothing ->
      Left ErrNoPaymentUTxOFound
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
  Map.lookupMax (Map.filter hasMarkerDatum utxo)
 where
  hasMarkerDatum :: TxOut -> Bool
  hasMarkerDatum (Ledger.Babbage.TxOut _ _ datum _) = case datum of
    NoDatum -> False
    DatumHash dh ->
      dh == hashData (Data @LedgerEra markerDatum)
    Datum{} -> False -- Marker is not stored inline

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
  Either (RdmrPtr, TransactionScriptFailure StandardCrypto) (Map RdmrPtr ExUnits)
estimateScriptsCost pparams systemStart epochInfo utxo tx = do
  -- FIXME: throwing exceptions in pure code is discouraged! Convert them to
  -- throwM or throwIO or represent thes situations in the return type!
  case result of
    Left translationError ->
      throw $ BadTranslationException translationError
    Right units ->
      Map.traverseWithKey (\ptr -> left (ptr,)) units
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

newtype BadTranslationException
  = BadTranslationException (TranslationError StandardCrypto)
  deriving (Show)

instance Exception BadTranslationException

--
-- Logs
--

data TinyWalletLog
  = InitializingWallet QueryPoint (Map TxIn TxOut)
  | ApplyBlock (Map TxIn TxOut)
  | LedgerEraMismatchError {expected :: Text, actual :: Text}
  deriving (Eq, Generic, Show)

instance ToJSON TinyWalletLog where
  toJSON =
    \case
      (InitializingWallet point initialUTxO) ->
        object
          [ "tag" .= String "InitializingWallet"
          , "point" .= show @Text point
          , "initialUTxO" .= initialUTxO
          ]
      (ApplyBlock utxo') ->
        object
          [ "tag" .= String "ApplyBlock"
          , "newUTxO" .= utxo'
          ]
      LedgerEraMismatchError{expected, actual} ->
        object
          [ "tag" .= String "EraMismatchError"
          , "expected" .= expected
          , "actual" .= actual
          ]

instance Arbitrary TinyWalletLog where
  arbitrary = genericArbitrary
