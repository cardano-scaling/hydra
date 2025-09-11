module Hydra.Tx.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (AllegraEraTxBody (vldtTxBodyL), ValidityInterval (..), bodyTxL, inputsTxBodyL, outputsTxBodyL)
import Control.Lens ((.~), (^.))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx (CommitBlueprintTx (..), HeadId, currencySymbolToHeadId, headIdToCurrencySymbol, txId)
import Hydra.Tx.Utils (addMetadata, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (POSIXTime)

-- * Construction

-- | Builds a deposit transaction to lock funds into the v_deposit script.
depositTx ::
  NetworkId ->
  HeadId ->
  CommitBlueprintTx Tx ->
  -- | Slot to use as upper validity. Will mark the time of creation of the deposit.
  SlotNo ->
  -- | Deposit deadline from which onward the deposit can be recovered.
  UTCTime ->
  -- | Optional amount to create partial deposit
  Maybe Coin ->
  Map PolicyId PolicyAssets ->
  Tx
depositTx networkId headId commitBlueprintTx upperSlot deadline amount tokens =
  fromLedgerTx $
    toLedgerTx blueprintTx
      & addDepositInputs
      & bodyTxL . outputsTxBodyL
        .~ ( StrictSeq.singleton (toLedgerTxOut $ mkDepositOutput networkId headId utxoToDeposit deadline)
              <> returnToUser
           )
      & bodyTxL . vldtTxBodyL .~ ValidityInterval{invalidBefore = SNothing, invalidHereafter = SJust upperSlot}
      & addMetadata (mkHydraHeadV1TxName "DepositTx") blueprintTx
 where
  addDepositInputs tx =
    let newInputs = tx ^. bodyTxL . inputsTxBodyL <> Set.fromList (toLedgerTxIn . fst <$> depositInputs)
     in tx & bodyTxL . inputsTxBodyL .~ newInputs

  CommitBlueprintTx{lookupUTxO = depositUTxO, blueprintTx} = commitBlueprintTx

  (utxoToDeposit', leftoverUTxO) = maybe (depositUTxO, mempty) (capUTxO depositUTxO) amount

  tokensToDepositUTxO = pickTokensToDeposit leftoverUTxO tokens

  utxoToDeposit = mergeUTxO utxoToDeposit' tokensToDepositUTxO

  returnToUser =
    let returnToUserUTxO = leftoverUTxO `filterExistingAssets` tokensToDepositUTxO
     in if UTxO.null returnToUserUTxO
          then StrictSeq.empty
          else
            let leftoverAddress = List.head $ txOutAddress <$> UTxO.txOutputs returnToUserUTxO
             in StrictSeq.singleton $
                  toLedgerTxOut $
                    TxOut leftoverAddress (UTxO.totalValue returnToUserUTxO) TxOutDatumNone ReferenceScriptNone

  depositInputsList = toList (UTxO.inputSet utxoToDeposit)

  depositInputs = (,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> depositInputsList

-- | Filter the first argument UTxO's non ADA assets in case any asset exists in the second UTxO argument.
-- Asset quantities will be subtracted if they are found.
filterExistingAssets :: UTxO -> UTxO -> UTxO
filterExistingAssets utxoToFilter utxoToLookup =
  UTxO.fromList $ findAssets <$> UTxO.toList utxoToFilter
 where
  findAssets (i, TxOut a val d r) =
    let assets = valueToPolicyAssets val
        originalLovelace = selectLovelace val
        filteredAssets =
         foldMap (uncurry policyAssetsToValue) $
          filterAssets assets forLookup
     in (i, TxOut a (lovelaceToValue originalLovelace <> filteredAssets) d r)
  forLookup =
     -- NOTE: Uses a list to store all policies, preserving multiple entries
     -- with the same policyId but different assets. A Map would silently
     -- overwrite duplicates.
     concatMap (Map.toList . valueToPolicyAssets . txOutValue . snd) $ UTxO.toList utxoToLookup

 -- | Filter the first argument map of assets in case any asset exists in the second argument.
filterAssets :: Map PolicyId PolicyAssets -> [(PolicyId, PolicyAssets)] -> [(PolicyId, PolicyAssets)]
filterAssets assets forLookup =
     Map.assocs $
       Map.mapWithKey
         ( \pid (PolicyAssets x) ->
             let samePid = List.filter (\(pid', _) -> pid' == pid) forLookup
              in case List.lookup pid samePid of
                   Nothing -> PolicyAssets x
                   Just (PolicyAssets foundAssets) -> PolicyAssets $ x `Map.difference` foundAssets
         )
         assets

-- | Merges the two 'UTxO' favoring data coming from the first argument 'UTxO'.
-- In case the same 'TxIn' was found in the first 'UTxO' - second 'UTxO' value
-- will be appended to the input.
-- NOTE: We need this since mappend on 'UTxO' is happy to accept the first 'Value' it encounters
-- in case 'TxId'/s are the same.
mergeUTxO :: UTxO -> UTxO -> UTxO
mergeUTxO utxo utxoToMerge =
  let existingInsAndOuts = UTxO.toList utxo
   in UTxO.fromList $
        List.foldl'
          ( \result newPair@(newTxIn, TxOut _ newVal _ _) ->
              case List.find (\(txin, _) -> txin == newTxIn) result of
                Nothing -> newPair : result
                Just (txIn, TxOut addr existingVal d r) ->
                  let assetsToInclude =
                        foldMap (uncurry policyAssetsToValue) (Map.assocs (valueToPolicyAssets newVal))
                      lovelaceToInclude = lovelaceToValue $ selectLovelace newVal
                   in [(txIn, TxOut addr (existingVal <> lovelaceToInclude <> assetsToInclude) d r)]
          )
          existingInsAndOuts
          (UTxO.toList utxoToMerge)

pickTokensToDeposit :: UTxO -> Map PolicyId PolicyAssets -> UTxO
pickTokensToDeposit leftoverUTxO depositTokens
  | Map.null depositTokens = mempty
  | otherwise = UTxO.fromList picked
 where
  -- Build list of (TxIn, new TxOut) where new TxOut has exact required quantities of matched assets.
  picked :: [(TxIn, TxOut CtxUTxO)]
  picked =
    [ (i, mkTxOutValueNotKeepingLovelace o newValue)
    | (i, o) <- UTxO.toList leftoverUTxO
    , let outputAssets = valueToPolicyAssets (txOutValue o)
    , let pickedPolicyAssets = pickMatchedAssets outputAssets depositTokens
    , not (Map.null pickedPolicyAssets)
    , let newValue = foldMap (uncurry policyAssetsToValue) (Map.toList pickedPolicyAssets)
    ]

  -- For a given output's assets and the required depositTokens, build a map of matched policies/assets (exact required qty).
  pickMatchedAssets :: Map PolicyId PolicyAssets -> Map PolicyId PolicyAssets -> Map PolicyId PolicyAssets
  pickMatchedAssets outputAssets = Map.foldrWithKey go mempty
   where
    go :: PolicyId -> PolicyAssets -> Map PolicyId PolicyAssets -> Map PolicyId PolicyAssets
    go pid (PolicyAssets requiredAssets) acc = case Map.lookup pid outputAssets of
      Nothing -> acc
      Just (PolicyAssets availAssets) ->
        let matchedAssets = Map.foldrWithKey (matchAsset availAssets) mempty requiredAssets
         in if Map.null matchedAssets then acc else Map.insert pid (PolicyAssets matchedAssets) acc

    matchAsset :: Map AssetName Quantity -> AssetName -> Quantity -> Map AssetName Quantity -> Map AssetName Quantity
    matchAsset availAssets name reqQty matched = case Map.lookup name availAssets of
      Just availQty | reqQty <= availQty -> Map.insert name reqQty matched
      _ -> matched

-- Helper to create TxOut with new value (unchanged from original) and removing all lovelace.
mkTxOutValueNotKeepingLovelace :: TxOut ctx -> Value -> TxOut ctx
mkTxOutValueNotKeepingLovelace (TxOut addr _ datum refScript) newValue =
  TxOut addr newValue datum refScript

mkDepositOutput ::
  NetworkId ->
  HeadId ->
  UTxO ->
  UTCTime ->
  TxOut ctx
mkDepositOutput networkId headId depositUTxO deadline =
  TxOut
    (depositAddress networkId)
    depositValue
    depositDatum
    ReferenceScriptNone
 where
  depositValue = UTxO.totalValue depositUTxO

  deposits = mapMaybe Commit.serializeCommit $ UTxO.toList depositUTxO

  depositPlutusDatum = Deposit.datum (headIdToCurrencySymbol headId, posixFromUTCTime deadline, deposits)

  depositDatum = mkTxOutDatumInline depositPlutusDatum

depositAddress :: NetworkId -> AddressInEra
depositAddress networkId = mkScriptAddress networkId depositValidatorScript

-- | Splits the specified tokens into those that are available in sufficient quantities in the user's UTxO
-- and those that are not (either because the policy is missing or the asset quantities are insufficient).
--
-- This function takes a user's UTxO and a map of specified policy IDs to their desired assets and quantities.
-- It checks the total value in the UTxO, converts it to a policy-assets map, and then partitions the specified
-- tokens into "ok" (available) and "invalid" (unavailable or insufficient).
--
-- If no tokens are specified, it returns two empty maps. Otherwise, it returns the partitioned maps, ensuring
-- that if there are no invalid tokens, the second map is empty.
splitTokens :: UTxO.UTxO -> Map PolicyId PolicyAssets -> (Map PolicyId PolicyAssets, Map PolicyId PolicyAssets)
splitTokens userUTxO specifiedTokens
  | Map.null specifiedTokens = (mempty, mempty) -- Trivial case: no tokens specified
  | otherwise =
      let utxoValue = UTxO.totalValue userUTxO
          existingPoliciesAndAssets = valueToPolicyAssets utxoValue
          (ok, invalid) = checkForTokens existingPoliciesAndAssets specifiedTokens
       in if Map.null invalid
            then (ok, mempty)
            else (ok, invalid)
 where
  checkForTokens ::
    Map PolicyId PolicyAssets ->
    Map PolicyId PolicyAssets ->
    (Map PolicyId PolicyAssets, Map PolicyId PolicyAssets)
  checkForTokens existing = Map.partitionWithKey (\k v -> checkAssets (Map.lookup k existing) v)

  checkAssets :: Maybe PolicyAssets -> PolicyAssets -> Bool
  checkAssets Nothing _ = False -- Policy not found in UTXO
  checkAssets (Just (PolicyAssets existingAssets)) (PolicyAssets wantedAssets) =
    all
      ( \(assetName, requiredQty) ->
          case Map.lookup assetName existingAssets of
            Nothing -> False
            Just availableQty -> availableQty >= requiredQty
      )
      (Map.toList wantedAssets)

-- | Caps a UTxO set to a specified target amount of Lovelace, splitting outputs if necessary.
--
-- Given a 'UTxO' set and a target 'Coin' value (in Lovelace), this function selects unspent transaction outputs
-- (UTxOs) to form a subset that sums as close as possible to the target value without exceeding it. If an output
-- needs to be split to meet the target exactly, it creates two new outputs: one contributing to the target and
-- another for the remaining value. The function ensures that the total Lovelace in the selected outputs does not
-- exceed the target.
--
-- === Algorithm
-- 1. **Base Cases**:
--    - If the target is 0, return an empty 'UTxO' as the selected set and the original 'UTxO' as leftovers.
--    - If the input 'UTxO' is empty, return two empty 'UTxO' sets.
-- 2. **Main Logic**:
--    - Sort the UTxO entries by their Lovelace value (ascending) to prioritize smaller outputs for efficiency.
--    - Use a recursive helper function 'go' to iterate through the sorted UTxO list, accumulating outputs until
--      the target value is reached or no suitable outputs remain.
--    - For each output:
--      - If adding the output's Lovelace value does not exceed the target, include it fully in the selected set
--        and remove it from the leftovers.
--      - If adding the output would exceed the target, split the output into two parts:
--        - One part contributes exactly the remaining amount needed to reach the target.
--        - The other part holds the excess Lovelace.
--        - Reuse 'TxIn' identifiers for the split outputs.
--      - Continue processing until the target is met or no outputs remain.
-- 3. **Termination**:
--    - The function stops when the accumulated Lovelace equals the target or when no more outputs are available.
--    - Returns a pair of 'UTxO' sets: the selected outputs (summing to at most the target) and the remaining outputs.
capUTxO :: UTxO -> Coin -> (UTxO, UTxO)
capUTxO utxo target
  | target == 0 = (mempty, utxo)
  | UTxO.null utxo = (mempty, mempty)
  | otherwise = go mempty utxo 0 (sortBy (comparing (selectLovelace . txOutValue . snd)) (UTxO.toList utxo))
 where
  -- \| Helper function to recursively select and split UTxO outputs to reach the target value.
  go foundSoFar leftovers currentSum sorted
    | currentSum == target = (foundSoFar, leftovers)
    | otherwise = case sorted of
        [] -> (foundSoFar, leftovers)
        (txIn, txOut) : rest ->
          let txOutLovelace = selectLovelace (txOutValue txOut)
              txOutAssetsVal = foldMap (uncurry policyAssetsToValue) (Map.toList $ valueToPolicyAssets (txOutValue txOut))
              newSum = currentSum + txOutLovelace
           in if newSum <= target
                then
                  -- Include the entire output if it doesn't exceed the target.
                  go
                    (foundSoFar <> UTxO.singleton txIn txOut)
                    (UTxO.difference leftovers $ UTxO.singleton txIn txOut)
                    newSum
                    rest
                else
                  -- Split the output to meet the target exactly.
                  let cappedValue = target - currentSum
                      leftoverVal = txOutLovelace - cappedValue
                      cappedTxOut = updateTxOutAdaValue txOut cappedValue
                      leftoverTxOut = updateTxOutValue txOut (lovelaceToValue leftoverVal <> txOutAssetsVal)
                   in go
                        (foundSoFar <> UTxO.singleton txIn cappedTxOut)
                        (UTxO.difference leftovers (UTxO.singleton txIn txOut) <> UTxO.singleton txIn leftoverTxOut)
                        target
                        rest

-- | Helper to create a new TxOut with specified lovelace value
updateTxOutAdaValue :: TxOut ctx -> Coin -> TxOut ctx
updateTxOutAdaValue (TxOut addr _ datum refScript) newValue =
  TxOut addr (fromLedgerValue $ mkAdaValue ShelleyBasedEraConway newValue) datum refScript

-- | Helper to create a new TxOut with specified 'Value'
updateTxOutValue :: TxOut ctx -> Value -> TxOut ctx
updateTxOutValue (TxOut addr _ datum refScript) newValue =
  TxOut addr newValue datum refScript

-- * Observation

data DepositObservation = DepositObservation
  { headId :: HeadId
  , depositTxId :: TxId
  , deposited :: UTxO
  , created :: SlotNo
  , deadline :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Observe a deposit transaction by decoding the target head id, deposit
-- deadline and deposited utxo in the datum.
--
-- This includes checking whether
-- - the first output is a deposit output
-- - all inputs of deposited utxo are actually spent,
-- - the deposit script output actually contains the deposited value,
-- - an upper validity bound has been set (used as creation slot).
observeDepositTx ::
  NetworkId ->
  Tx ->
  Maybe DepositObservation
observeDepositTx networkId tx = do
  depositOut <- fmap head . nonEmpty $ txOuts' tx
  (headId, deposited, deadline) <- observeDepositTxOut network (toCtxUTxOTxOut depositOut)
  guard $ spendsAll deposited
  created <- getUpperBound
  pure
    DepositObservation
      { headId
      , depositTxId = txId tx
      , deposited
      , created
      , deadline = posixToUTCTime deadline
      }
 where
  spendsAll = all (`elem` txIns' tx) . UTxO.inputSet

  getUpperBound =
    case tx & getTxBody & getTxBodyContent & txValidityUpperBound of
      TxValidityUpperBound{upperBound} -> Just upperBound
      TxValidityNoUpperBound -> Nothing

  network = toShelleyNetwork networkId

observeDepositTxOut :: Network -> TxOut CtxUTxO -> Maybe (HeadId, UTxO, POSIXTime)
observeDepositTxOut network depositOut = do
  dat <- case txOutDatum depositOut of
    TxOutDatumInline d -> pure d
    _ -> Nothing
  (headCurrencySymbol, deadline, onChainDeposits) <- fromScriptData dat
  headId <- currencySymbolToHeadId headCurrencySymbol
  deposit <- do
    depositedUTxO <- UTxO.fromList <$> traverse (Commit.deserializeCommit network) onChainDeposits
    guard $ depositValue `containsValue` UTxO.totalValue depositedUTxO
    pure depositedUTxO
  pure (headId, deposit, deadline)
 where
  depositValue = txOutValue depositOut
