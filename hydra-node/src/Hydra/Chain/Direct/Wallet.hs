{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Companion tiny-wallet for the direct chain component. This module provide
-- some useful utilities to tracking the wallet's UTXO, and accessing it
module Hydra.Chain.Direct.Wallet where

import Hydra.Prelude

import Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Alonzo.Data (Data (Data))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (language)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (Spend), txscriptfee)
import Cardano.Ledger.Alonzo.Tools (
  BasicFailure (..),
  ScriptFailure (..),
  evaluateTransactionExecutionUnits,
 )
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..), hashData, hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxBody (
  collateral,
  inputs,
  outputs,
  scriptIntegrityHash,
  txfee,
  pattern TxOut,
 )
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..))
import Cardano.Ledger.Alonzo.TxWitness (
  RdmrPtr (RdmrPtr),
  Redeemers (..),
  TxWitness (..),
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (PParams)
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (HASH, StandardCrypto)
import Cardano.Ledger.Era (ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import qualified Cardano.Ledger.Keys as Ledger
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
import Data.Array (Array, array)
import qualified Data.List as List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Ix (Ix)
import Hydra.Cardano.Api (
  CardanoMode,
  EraHistory (EraHistory),
  NetworkId,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  VerificationKey,
  makeShelleyAddress,
  shelleyAddressInEra,
  shelleyBasedEra,
  toLedgerAddr,
  toLedgerPParams,
  toLedgerUTxO,
  verificationKeyHash,
 )
import qualified Hydra.Cardano.Api as Api
import Hydra.Chain.CardanoClient (queryEraHistory, queryProtocolParameters, querySystemStart, queryUTxO)
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  SomePoint (..),
  markerDatum,
 )
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Logging (Tracer, traceWith)
import Ouroboros.Consensus.Cardano.Block (CardanoEras, pattern BlockAlonzo)
import Ouroboros.Consensus.HardFork.Combinator (MismatchEraInfo)
import Ouroboros.Consensus.HardFork.History (PastHorizonException)
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Ouroboros.Network.Block (Point (..))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

type Address = Ledger.Addr StandardCrypto
type TxBody = Ledger.TxBody Era
type TxIn = Ledger.TxIn StandardCrypto
type TxOut = Ledger.TxOut Era
type VkWitness = Ledger.WitVKey 'Ledger.Witness StandardCrypto
type QueryResult result = Either (MismatchEraInfo (CardanoEras StandardCrypto)) result
type UTxOSet = Ledger.UTxO Era
type AlonzoPoint = Point (ShelleyBlock Era)

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
  , getAddress :: Address
  , sign :: ValidatedTx Era -> ValidatedTx Era
  , coverFee :: Map TxIn TxOut -> ValidatedTx Era -> STM m (Either ErrCoverFee (ValidatedTx Era))
  , verificationKey :: VerificationKey PaymentKey
  , -- | Reset the wallet state to some point.
    reset :: Maybe (Point Block) -> m ()
  , -- | Update the wallet state given some 'Block'.
    update :: Block -> m ()
  }

-- | Get a single, marked as "fuel" UTxO.
getFuelUTxO :: MonadSTM m => TinyWallet m -> STM m (Maybe (TxIn, TxOut))
getFuelUTxO TinyWallet{getUTxO} =
  findFuelUTxO <$> getUTxO

watchUTxOUntil :: (Map TxIn TxOut -> Bool) -> TinyWallet IO -> IO (Map TxIn TxOut)
watchUTxOUntil predicate TinyWallet{getUTxO} = atomically $ do
  u <- getUTxO
  u <$ check (predicate u)

withTinyWallet ::
  -- | A tracer for logging
  Tracer IO TinyWalletLog ->
  -- | Network identifier to which we expect to connect.
  NetworkId ->
  -- | Credentials of the wallet.
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | Path to a domain socket used to connect to the server.
  FilePath ->
  (TinyWallet IO -> IO a) ->
  IO a
withTinyWallet tracer networkId (vk, sk) socketPath action = do
  utxoVar <- newTVarIO =<< queryUTxOEtc
  action $ newTinyWallet utxoVar
 where
  queryUTxOEtc = do
    utxo <- Ledger.unUTxO . toLedgerUTxO <$> queryUTxO networkId socketPath [address]
    pparams <- toLedgerPParams (shelleyBasedEra @Api.Era) <$> queryProtocolParameters networkId socketPath
    systemStart <- querySystemStart networkId socketPath
    epochInfo <- toEpochInfo <$> queryEraHistory networkId socketPath
    pure (utxo, pparams, systemStart, epochInfo)

  address =
    makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress

  ledgerAddress = toLedgerAddr $ shelleyAddressInEra @Api.Era address

  newTinyWallet utxoVar =
    TinyWallet
      { getUTxO =
          (\(u, _, _, _) -> u) <$> readTVar utxoVar
      , getAddress = ledgerAddress
      , sign = Util.signWith (vk, sk)
      , coverFee = \lookupUTxO partialTx -> do
          (walletUTxO, pparams, systemStart, epochInfo) <- readTVar utxoVar
          case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO partialTx of
            Left e ->
              pure (Left e)
            Right (walletUTxO', balancedTx) -> do
              writeTVar utxoVar (walletUTxO', pparams, systemStart, epochInfo)
              pure (Right balancedTx)
      , verificationKey = vk
      , reset = \_mPoint -> do
          -- TODO: query from point?
          traceWith tracer ResetWallet
          queryUTxOEtc >>= atomically . writeTVar utxoVar
      , update = \block -> do
          msg <- atomically $ do
            (utxo, pparams, systemStart, epochInfo) <- readTVar utxoVar
            let utxo' = applyBlock block (== ledgerAddress) utxo
            if utxo' /= utxo
              then do
                writeTVar utxoVar (utxo', pparams, systemStart, epochInfo)
                pure $ Just $ ApplyBlock utxo utxo'
              else do
                pure Nothing
          mapM_ (traceWith tracer) msg
      }

  toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Except PastHorizonException)
  toEpochInfo (EraHistory _ interpreter) =
    -- hoistEpochInfo (either throwIO pure . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

-- | Apply a block to our wallet. Does nothing if the transaction does not
-- modify the UTXO set, or else, remove consumed utxos and add produced ones.
--
-- To determine whether a produced output is ours, we apply the given function
-- checking the output's address.
applyBlock :: Block -> (Address -> Bool) -> Map TxIn TxOut -> Map TxIn TxOut
applyBlock blk isOurs utxo = case blk of
  BlockAlonzo (ShelleyBlock (Ledger.Block _ bbody) _) ->
    flip execState utxo $ do
      forM_ (txSeqTxns bbody) $ \tx -> do
        let txId = getTxId tx
        modify (`Map.withoutKeys` inputs (body tx))
        let indexedOutputs =
              let outs = outputs (body tx)
               in StrictSeq.zip (StrictSeq.fromList [0 .. length outs]) outs
        forM_ indexedOutputs $ \(fromIntegral -> ix, out@(TxOut addr _ _)) ->
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
  | ErrScriptExecutionFailed (RdmrPtr, ScriptFailure StandardCrypto)
  deriving (Show)

data ChangeError = ChangeError {inputBalance :: Coin, outputBalance :: Coin}
  deriving (Show)

-- | Cover fee for a transaction body using the given UTXO set. This calculate
-- necessary fees and augments inputs / outputs / collateral accordingly to
-- cover for the transaction cost and get the change back.
--
-- TODO: The fee calculation is currently very dumb and static.
coverFee_ ::
  PParams Era ->
  SystemStart ->
  EpochInfo (Except PastHorizonException) ->
  Map TxIn TxOut ->
  Map TxIn TxOut ->
  ValidatedTx Era ->
  Either ErrCoverFee (Map TxIn TxOut, ValidatedTx Era)
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
        , (not . isNativeScript @Era) script
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
  getAdaValue (TxOut _ value _) =
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
  mkChange (TxOut addr _ datum) resolvedInputs otherOutputs fee
    -- FIXME: The delta between in and out must be greater than the min utxo value!
    | totalIn <= totalOut =
      Left $
        ChangeError
          { inputBalance = totalIn
          , outputBalance = totalOut
          }
    | otherwise =
      Right $ TxOut addr (inject changeOut) datum
   where
    totalOut = foldMap getAdaValue otherOutputs <> fee
    totalIn = foldMap getAdaValue resolvedInputs
    changeOut = totalIn <> invert totalOut

  adjustRedeemers :: Set TxIn -> Set TxIn -> Map RdmrPtr ExUnits -> Redeemers Era -> Redeemers Era
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
  hasMarkerDatum (TxOut _ _ dh) =
    dh == SJust (hashData $ Data @Era markerDatum)

-- | Estimate cost of script executions on the transaction. This is only an
-- estimates because the transaction isn't sealed at this point and adding new
-- elements to it like change outputs or script integrity hash may increase that
-- cost a little.
estimateScriptsCost ::
  -- | Protocol parameters
  PParams Era ->
  -- | Start of the blockchain, for converting slots to UTC times
  SystemStart ->
  -- | Information about epoch sizes, for converting slots to UTC times
  EpochInfo (Except PastHorizonException) ->
  -- | A UTXO needed to resolve inputs
  Map TxIn TxOut ->
  -- | The pre-constructed transaction
  ValidatedTx Era ->
  Either (RdmrPtr, ScriptFailure StandardCrypto) (Map RdmrPtr ExUnits)
estimateScriptsCost pparams systemStart epochInfo utxo tx = do
  case result of
    Left pastHorizonException ->
      throw pastHorizonException
    Right (Left (UnknownTxIns ins)) ->
      throw (UnknownTxInsException ins)
    Right (Right units) ->
      Map.traverseWithKey (\ptr -> left (ptr,)) units
 where
  result =
    runIdentity $
      runExceptT $
        evaluateTransactionExecutionUnits
          pparams
          tx
          (Ledger.UTxO utxo)
          epochInfo
          systemStart
          (mapToArray (_costmdls pparams))

newtype UnknownTxInsException
  = UnknownTxInsException (Set TxIn)
  deriving (Show)

instance Exception UnknownTxInsException

mapToArray :: Ix k => Map k v -> Array k v
mapToArray m =
  array
    (fst (Map.findMin m), fst (Map.findMax m))
    (Map.toList m)

--
-- Logs
--

data TinyWalletLog
  = InitializingWallet SomePoint (Map TxIn TxOut)
  | ResetWallet
  | ApplyBlock (Map TxIn TxOut) (Map TxIn TxOut)
  | EraMismatchError {expected :: Text, actual :: Text}
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
      ResetWallet -> object ["tag" .= String "ResetWallet"]
      (ApplyBlock utxo utxo') ->
        object
          [ "tag" .= String "ApplyBlock"
          , "before" .= utxo
          , "after" .= utxo'
          ]
      EraMismatchError{expected, actual} ->
        object
          [ "tag" .= String "EraMismatchError"
          , "expected" .= expected
          , "actual" .= actual
          ]

instance Arbitrary TinyWalletLog where
  arbitrary = genericArbitrary
