{-# LANGUAGE OverloadedRecordDot #-}

-- | Differential tests (Tier 2), generalizing 'Hydra.Tx.Contract.CloseDifferential' to the
-- increment, decrement, contest and fanout transaction families: run the Agda-extracted decidable
-- reference checkers ('Hydra.Agda.Reference') and the real Plutus validator on the same
-- transactions and assert @reference-rejects ⇒ validator-rejects@.
--
-- Each reference checker mirrors only the decidable conjuncts of the corresponding
-- @*Valid@ bundle (proved to reflect them in spec/src/Hydra/Protocol/ReferenceBridge.agda):
--
--   * increment\/decrement: produced version is @suc@ the input version (@VersionNotIncremented@);
--   * contest: version preserved, snapshot strictly increases (@TooOldSnapshot@), one contester
--     appended, and posted before the contestation deadline (@validityHi ≤ tfinal@,
--     @mustBeWithinContestationPeriod@);
--   * fanout\/finalPartialFanout: all @n+1@ head tokens burned (@BurntTokenNumberMismatch@) and posted
--     after the deadline (@tfinal < lo@, @LowerBoundBeforeContestationDeadline@). NB the full fanout
--     permits @m == 0@ (finalising an empty head), so no @0 < m@ conjunct is checked.
--
-- The remaining crypto\/value\/accumulator conjuncts are mocked (@const True@) on the reference side,
-- so the converse direction (validator-rejects-while-reference-accepts) is expected and not
-- asserted.
module Hydra.Tx.Contract.Differential (spec, participantSignedRef, noMintRef, refSpentRef) where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Maybe (fromJust)
import GHC.IsList qualified as IsList
import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (
  AssetId (..),
  Coin (..),
  Quantity (..),
  Tx,
  TxIn,
  TxOut,
  UTxO,
  fromCtxUTxOTxOut,
  getTxBody,
  getTxBodyContent,
  modifyTxOutValue,
  resolveInputsUTxO,
  selectLovelace,
  serialiseToRawBytes,
  toPlutusCurrencySymbol,
  toPlutusTxOutRef,
  txExtraKeyWits,
  txIns,
  txMintValue,
  txMintValueToValue,
  txOutValue,
  txOuts',
  txValidityLowerBound,
  txValidityUpperBound,
  pattern TxExtraKeyWitnesses,
  pattern TxValidityLowerBound,
  pattern TxValidityUpperBound,
  pattern UnsafeAssetName,
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Cardano.Api.TxBody (findRedeemerSpending)
import Hydra.Cardano.Api.TxOut (findTxOutByScript, findTxOutsByScript)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as HS
import Hydra.Data.ContestationPeriod (milliseconds)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx.Contract.Contest.ContestCurrent (genContestMutation)
import Hydra.Tx.Contract.Contest.Healthy (healthyContestTx)
import Hydra.Tx.Contract.Decrement (genDecrementMutation, healthyDecrementTx)
import Hydra.Tx.Contract.FanOut (genFanoutMutation, healthyFanoutTx)
import Hydra.Tx.Contract.Increment (genIncrementMutation, healthyIncrementTx)
import Hydra.Tx.Contract.PartialFanout (genPartialFanoutMutation, healthyIntermediatePartialFanoutTx)
import Hydra.Tx.Utils (hydraHeadV2AssetName)
import PlutusLedgerApi.V3 (TxId (..), TxOutRef (..), fromBuiltin, getPOSIXTime)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, slotLength, systemStart)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (testPolicyId)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), applyMutation)
import Test.QuickCheck (Property, forAll, property, (===))

-- | Does the real Plutus validator accept @(tx, utxo)@ (phase-2 success, no script error)?
validatorAccepts :: (Tx, UTxO) -> Bool
validatorAccepts (tx, utxo) =
  case evaluateTx tx utxo of
    Right report -> all isRight report
    Left _ -> False

-- | The spent head datum (continuing input) for @(tx, utxo)@.
inputState :: (Tx, UTxO) -> Maybe (HS.State, TxIn)
inputState (_, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  st <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  pure (st, headIn)

-- | The produced (continuing) head datum at output index 0, if any.
outputState :: (Tx, UTxO) -> Maybe HS.State
outputState (tx, _) = do
  headOut <- txOuts' tx !!? 0
  fromScriptData =<< txOutScriptData headOut

-- ── increment / decrement ─────────────────────────────────────────────────────────────────
-- Increment steps Open→Open; the reference now checks the version bump AND lovelace value
-- conservation (adaIn + adaDelta == adaOut), reading the real head-input / deposit / head-output
-- lovelace off the tx — so a value-breaking validator change is caught, not just version drift.
incRefVerdict :: (Tx, UTxO) -> Maybe Bool
incRefVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  inSt <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  HS.Open od <- Just (inSt :: HS.State)
  headOut <- txOuts' tx !!? 0
  outSt <- fromScriptData =<< txOutScriptData headOut
  HS.Open od' <- Just (outSt :: HS.State)
  HS.Increment HS.IncrementRedeemer{HS.increment} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  -- Sum EVERY spent deposit input (not just the redeemer's claimed one): mirrors the Agda
  -- `depositsValue` / Plutus `totalNonHeadInputValue`, so a multi-deposit siphon (an extra deposit
  -- whose value is routed away) makes adaIn + Σdeposits ≠ adaOut and the reference rejects.
  let deposits = snd <$> findTxOutsByScript (resolveInputsUTxO utxo tx) depositValidatorScript
      depositLovelace = sum (lovelace <$> deposits)
      depositNonAda = sum (nonAda <$> deposits)
  pure $
    Ref.checkInc
      (Ref.mkOpsInc (const True))
      ( Ref.MkIncIO
          od.version
          od'.version
          (lovelace headInOut)
          depositLovelace
          (lovelace headOut)
          (nonAda headInOut)
          depositNonAda
          (nonAda headOut)
      )
      -- §5.4 mustBeSignedByParticipant: some signer holds a PT in the head output (shared checker).
      && participantSignedRef tx headOut
      -- §5.4 mustNotMintOrBurn: the increment mints/burns nothing (shared checker).
      && noMintRef tx
      -- §5.4 claimedDepositIsSpent: the redeemer's claimed deposit ref is among the spent inputs.
      && refSpentRef tx increment

-- | Lovelace (ada) component of a tx output's value, as the plain Integer the reference boundary takes.
lovelace :: TxOut ctx -> Integer
lovelace o = let Coin n = selectLovelace (txOutValue o) in n

-- | Total NON-ada token quantity of a tx output's value (the `nonAdaOf` projection): the sum of all
-- native-asset quantities, ada excluded. Checking conservation of this alongside 'lovelace' catches a
-- native-token siphon that an ada-only check misses.
nonAda :: TxOut ctx -> Integer
nonAda o = sum [q | (aid, Quantity q) <- IsList.toList (txOutValue o), aid /= AdaAssetId]

-- | Quantity of one specific asset in a tx output's value (0 if absent), as the Integer the per-asset
-- reference takes — the `quantityOfᴺ` projection.
quantityOfAsset :: TxOut ctx -> AssetId -> Integer
quantityOfAsset o a = sum [q | (aid, Quantity q) <- IsList.toList (txOutValue o), aid == a]

-- | The native (non-ada) asset ids present in a tx output's value.
nonAdaAssets :: TxOut ctx -> [AssetId]
nonAdaAssets o = [aid | (aid, _) <- IsList.toList (txOutValue o), aid /= AdaAssetId]

-- | Per-asset increment value-conservation verdict: for EVERY native asset across the head input, the
-- deposits and the head output, @quantityOfᴺ headIn + quantityOfᴺ deposits == quantityOfᴺ headOut@. This
-- refines the `nonAda` TOTAL check in 'incRefVerdict' to per (policy,token), so it catches a selective
-- single-token swap that leaves the total balanced (proved to reflect `incrementValueOK` per asset as
-- `incPerAsset→ref`). 'Nothing' if the tx is not a readable Open→Open increment.
incPerAssetVerdict :: (Tx, UTxO) -> Maybe Bool
incPerAssetVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  HS.Open _ <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut) :: Maybe HS.State
  headOut <- txOuts' tx !!? 0
  HS.Increment{} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  let deposits = snd <$> findTxOutsByScript (resolveInputsUTxO utxo tx) depositValidatorScript
      assets = nub (nonAdaAssets headInOut ++ concatMap nonAdaAssets deposits ++ nonAdaAssets headOut)
      ios =
        [ Ref.MkAssetIO
          (quantityOfAsset headInOut a)
          (sum [quantityOfAsset d a | d <- deposits])
          (quantityOfAsset headOut a)
        | a <- assets
        ]
  pure (Ref.checkPerAsset ios)

-- | The participant-signature inputs the shared 'Ref.checkParticipantSigned' reference takes, read off
-- the real tx: the signers' key-hashes (txInfoSignatories) and the head value's PT token-names. Both
-- sides are the SAME hash bytes (a PT's token name IS a participant's key-hash, cf. @onChainIdToAssetName@
-- / required signers) under one 'encodeHash' encoding, so they overlap exactly when a PT holder signed.
encodeHash :: ByteString -> Integer
encodeHash = BS.foldl' (\acc w -> acc * 256 + toInteger w) 0

-- | The tx's signing key-hashes (the required signers, which the ledger surfaces to the validator as
-- @txInfoSignatories@), each encoded as an Integer.
signerCodes :: Tx -> [Integer]
signerCodes tx =
  case txExtraKeyWits (getTxBodyContent (getTxBody tx)) of
    TxExtraKeyWitnesses hashes -> encodeHash . serialiseToRawBytes <$> hashes
    _ -> []

-- | The participation-token names in the head output value (its non-ada assets other than the state
-- token @hydraHeadV2AssetName@), each encoded as an Integer with the same 'encodeHash' as 'signerCodes'.
ptCodes :: TxOut ctx -> [Integer]
ptCodes o =
  [ encodeHash bs
  | (AssetId _ an@(UnsafeAssetName bs), _) <- IsList.toList (txOutValue o)
  , an /= hydraHeadV2AssetName
  ]

-- | The shared §5.4–5.7 @mustBeSignedByParticipant@ reference check on a tx and its head output: do the
-- tx signers' key-hashes overlap the head value's PT names? Exported so the close differential reuses it.
participantSignedRef :: Tx -> TxOut ctx -> Bool
participantSignedRef tx out = Ref.checkParticipantSigned (Ref.MkSignerIO (signerCodes tx) (ptCodes out))

-- | The number of non-zero asset entries in the tx's mint value; 0 exactly when nothing is minted or
-- burned. The shared @mustNotMintOrBurn@ reference (close\/contest\/increment\/decrement) accepts iff
-- this is 0, so any minting\/burning mutation makes it positive and the reference rejects.
noMintEntryCount :: Tx -> Integer
noMintEntryCount tx =
  fromIntegral (length [() | (_aid, Quantity q) <- IsList.toList mintV, q /= 0])
 where
  mintV = txMintValueToValue (txMintValue (getTxBodyContent (getTxBody tx)))

-- | The shared §5.4–5.7 @mustNotMintOrBurn@ reference check on a tx. Exported so the close differential
-- reuses it.
noMintRef :: Tx -> Bool
noMintRef = Ref.checkNoMint . noMintEntryCount

-- | A deterministic Integer encoding of a Plutus 'TxOutRef', so the @refSpent@ reference can compare the
-- redeemer's claimed out-ref against the tx's spent-input out-refs (both encoded the same way). The output
-- index is small, so @hash * K + idx@ is collision-free across a single tx's inputs.
encodeTxOutRef :: TxOutRef -> Integer
encodeTxOutRef (TxOutRef (TxId h) ix) = encodeHash (fromBuiltin h) * 100000 + ix

-- | The encodings of the tx's spent-input out-refs (each Cardano 'TxIn' converted to the Plutus
-- 'TxOutRef' the validator sees), under the same 'encodeTxOutRef' as the redeemer's claimed ref.
inputRefCodes :: Tx -> [Integer]
inputRefCodes tx = encodeTxOutRef . toPlutusTxOutRef . fst <$> txIns (getTxBodyContent (getTxBody tx))

-- | The shared "referenced output is spent" reference check: is @ref@ among the tx's spent inputs? Used
-- by the increment (the redeemer's claimed deposit) and the init differential (the head's seed). Exported.
refSpentRef :: Tx -> TxOutRef -> Bool
refSpentRef tx ref = Ref.checkRefSpent (encodeTxOutRef ref) (inputRefCodes tx)

-- Decrement steps Open→Open and SHRINKS the head value by the decommit. Like increment, the reference
-- now checks lovelace conservation (adaOut + adaDelta == adaIn): head output + the decommitted outputs
-- == head input. The decommitted outputs are @take numberOfDecommitOutputs (tail (txOuts' tx))@ (head
-- output is index 0), mirroring the validator's @decommitOutputs@ in @checkDecrement@.
decRefVerdict :: (Tx, UTxO) -> Maybe Bool
decRefVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  inSt <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  HS.Open od <- Just (inSt :: HS.State)
  headOut <- txOuts' tx !!? 0
  outSt <- fromScriptData =<< txOutScriptData headOut
  HS.Open od' <- Just (outSt :: HS.State)
  HS.Decrement HS.DecrementRedeemer{HS.numberOfDecommitOutputs} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  let decommitOuts = take (fromIntegral numberOfDecommitOutputs) (drop 1 (txOuts' tx))
      decommitLovelace = sum (lovelace <$> decommitOuts)
      decommitNonAda = sum (nonAda <$> decommitOuts)
  pure $
    Ref.checkDec
      (Ref.mkOpsInc (const True))
      ( Ref.MkIncIO
          od.version
          od'.version
          (lovelace headInOut)
          decommitLovelace
          (lovelace headOut)
          (nonAda headInOut)
          decommitNonAda
          (nonAda headOut)
      )
      -- §5.5 mustBeSignedByParticipant: some signer holds a PT in the head output (shared checker).
      && participantSignedRef tx headOut
      -- §5.5 mustNotMintOrBurn: the decrement mints/burns nothing (shared checker).
      && noMintRef tx

-- | Per-asset DECREMENT value-conservation verdict (mirror of 'incPerAssetVerdict'): for EVERY native
-- asset across the head input, the decommitted outputs and the head output,
-- @quantityOfᴺ headOut + quantityOfᴺ decommit == quantityOfᴺ headIn@. Refines the `nonAda` TOTAL in
-- 'decRefVerdict' to per (policy,token), catching a selective single-token over-decommit the total misses
-- (proved to reflect `decrementValueOK` per asset as `decPerAsset→ref`). The AssetIO order matches the
-- shared `perAssetConservedᵇ` sum-check: (qIn := headOut, qDelta := decommit, qOut := headIn).
decPerAssetVerdict :: (Tx, UTxO) -> Maybe Bool
decPerAssetVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  HS.Open _ <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut) :: Maybe HS.State
  headOut <- txOuts' tx !!? 0
  HS.Decrement HS.DecrementRedeemer{HS.numberOfDecommitOutputs} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  let decommitOuts = take (fromIntegral numberOfDecommitOutputs) (drop 1 (txOuts' tx))
      assets = nub (nonAdaAssets headInOut ++ concatMap nonAdaAssets decommitOuts ++ nonAdaAssets headOut)
      ios =
        [ Ref.MkAssetIO
          (quantityOfAsset headOut a)
          (sum [quantityOfAsset d a | d <- decommitOuts])
          (quantityOfAsset headInOut a)
        | a <- assets
        ]
  pure (Ref.checkPerAsset ios)

-- ── contest ─────────────────────────────────────────────────────────────────────────────────
-- The reference now also checks the before-deadline guard (@validityHi ≤ tfinal@) AND the conditional
-- deadline-UPDATE rule (@tfinal' == if all-contested then tfinal else tfinal+cp@, the validator's
-- @makeContestationDeadline@/@addContestationPeriod@): the produced deadline, party count @n@ and the
-- contestation period (ms) are read off the datums. (`mustBeWithinContestationPeriod` + the bump.)
contestRefVerdict :: (Tx, UTxO) -> Maybe Bool
contestRefVerdict m@(tx, _) = do
  (inSt, headIn) <- inputState m
  HS.Closed cd <- Just inSt
  outSt <- outputState m
  HS.Closed cd' <- Just outSt
  HS.Contest _ <- findRedeemerSpending tx headIn :: Maybe HS.Input
  validityHi <- txUpperBoundPOSIX tx
  headOut <- txOuts' tx !!? 0
  pure $
    Ref.checkContest
      (Ref.mkOpsContest (const True))
      ( Ref.MkContestIO
          cd.version
          cd'.version
          cd.snapshotNumber
          cd'.snapshotNumber
          (fromIntegral (length cd.contesters))
          (fromIntegral (length cd'.contesters))
          (getPOSIXTime cd.contestationDeadline)
          validityHi
          (getPOSIXTime cd'.contestationDeadline)
          (fromIntegral (length cd.parties))
          (toInteger (milliseconds cd.contestationPeriod))
      )
      -- §5.7 mustBeSignedByParticipant: some signer holds a PT in the head output (shared checker).
      && participantSignedRef tx headOut
      -- §5.7 mustNotMintOrBurn: the contest mints/burns nothing (shared checker).
      && noMintRef tx

-- ── fanout / finalPartialFanout ───────────────────────────────────────────────────────────────
-- The reference now checks, besides @0 < m@: the burn count (@burnedCount == n+1@, the negated
-- head-policy mint summed off the tx — mirror of the init mint count) and the after-deadline guard
-- (@tfinal < lo@, the tx lower validity bound as POSIXTime, mirror of the recover after-deadline).
fanoutRefVerdict :: (Tx, UTxO) -> Maybe Bool
fanoutRefVerdict m@(tx, _) = do
  (inSt, headIn) <- inputState m
  HS.Closed cd <- Just inSt
  HS.Fanout{HS.numberOfFanoutOutputs} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  validityLo <- txLowerBoundPOSIX tx
  let minted = IsList.toList (txMintValueToValue (txMintValue (getTxBodyContent (getTxBody tx))))
      -- burned head-policy tokens = negated sum of the head policy's (negative) mint quantities,
      -- mirroring the validator's `mustBurnAllHeadTokens` (`burntTokens == n+1`).
      burned = negate (sum [q | (AssetId pid _, Quantity q) <- minted, toPlutusCurrencySymbol pid == cd.headId])
  pure $
    Ref.checkFanout
      (Ref.mkOpsFanout (const True))
      ( Ref.MkFanout
          numberOfFanoutOutputs
          burned
          (fromIntegral (length cd.parties))
          (getPOSIXTime cd.contestationDeadline)
          validityLo
      )

-- ── non-final partial fanout (FanoutProgress → FanoutProgress) ──────────────────────────────────
-- The reference checks the decidable conjuncts of the intermediate partial-fanout batch: at least one
-- output distributed (@0 < m@, the @PartialFanoutZeroOutputs@ guard the full fanout omits), posted after
-- the deadline (@tfinal < lo@), and (shared) @mustNotMintOrBurn@. The accumulator-membership and
-- value-conservation conjuncts stay mocked. 'Nothing' if the tx is not a readable FanoutProgress→… batch.
partialFanoutRefVerdict :: (Tx, UTxO) -> Maybe Bool
partialFanoutRefVerdict m@(tx, _) = do
  (inSt, headIn) <- inputState m
  HS.FanoutProgress fpd <- Just inSt
  HS.PartialFanout{HS.numberOfPartialOutputs} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  validityLo <- txLowerBoundPOSIX tx
  pure $
    Ref.checkPartialFanout
      numberOfPartialOutputs
      (getPOSIXTime fpd.contestationDeadline)
      validityLo
      -- §5.8 mustNotMintOrBurn: the partial fanout mints/burns nothing (shared checker).
      && noMintRef tx

-- | The tx lower validity bound as POSIXTime milliseconds (matching the ledger's slot→time translation
-- under the linear `fixedEpochInfo` fixture; see the 'Hydra.Tx.Contract.CloseDifferential' note), or
-- 'Nothing' if there is no finite lower bound.
txLowerBoundPOSIX :: Tx -> Maybe Integer
txLowerBoundPOSIX tx =
  case tx & getTxBody & getTxBodyContent & txValidityLowerBound of
    TxValidityLowerBound lowerBound -> Just (getPOSIXTime (posixFromUTCTime (slotNoToUTCTime systemStart slotLength lowerBound)))
    _ -> Nothing

-- | The tx upper validity bound as POSIXTime milliseconds (same slot→time translation), or 'Nothing'
-- if there is no finite upper bound (which the contest validator rejects as @InfiniteUpperBound@; the
-- reference then abstains).
txUpperBoundPOSIX :: Tx -> Maybe Integer
txUpperBoundPOSIX tx =
  case tx & getTxBody & getTxBodyContent & txValidityUpperBound of
    TxValidityUpperBound upperBound -> Just (getPOSIXTime (posixFromUTCTime (slotNoToUTCTime systemStart slotLength upperBound)))
    _ -> Nothing

-- ── non-ada value-siphon demonstration ─────────────────────────────────────────────────────────

-- | The OLD ada-only conservation check (@adaIn + Σdeposit-ada == adaOut@), kept to show that a pure
-- native-token siphon SATISFIES it — so the previous lovelace-only reference would have accepted the
-- very tx the new non-ada conjunct rejects.
incLovelaceConserved :: (Tx, UTxO) -> Maybe Bool
incLovelaceConserved (tx, utxo) = do
  (_, headInOut) <- findTxOutByScript utxo Head.validatorScript
  headOut <- txOuts' tx !!? 0
  let depositLovelace = sum (lovelace . snd <$> findTxOutsByScript (resolveInputsUTxO utxo tx) depositValidatorScript)
  pure (lovelace headInOut + depositLovelace == lovelace headOut)

-- | The healthy increment with an extra NON-ada token grafted onto the head output (index 0). ADA is
-- untouched, so 'incLovelaceConserved' still holds, yet the new non-ada conjunct fails and the validator
-- rejects (@mustPreserveValue@): exactly the native-token siphon class an ada-only check missed.
tokenSiphonIncrementTx :: (Tx, UTxO)
tokenSiphonIncrementTx =
  applyMutation (ChangeOutput 0 (modifyTxOutValue (<> siphonToken) headOut)) healthyIncrementTx
 where
  headOut = fromJust (txOuts' (fst healthyIncrementTx) !!? 0)
  siphonToken = IsList.fromList [(AssetId testPolicyId (UnsafeAssetName "siphon"), 1)]

-- | The healthy increment with the head output's STATE token swapped 1-for-1 for an unrelated token: the
-- NON-ada TOTAL is unchanged (−1 ST, +1 swap), so the total-only check in 'incRefVerdict' still accepts,
-- yet the per-asset conjunct sees the ST short by one (and the swap token in excess) and the validator
-- rejects (@mustPreserveValue@). The selective swap a total-quantity check structurally cannot see.
balancedSwapIncrementTx :: (Tx, UTxO)
balancedSwapIncrementTx =
  applyMutation (ChangeOutput 0 (modifyTxOutValue (<> tokenSwap) headOut)) healthyIncrementTx
 where
  headOut = fromJust (txOuts' (fst healthyIncrementTx) !!? 0)
  tokenSwap =
    IsList.fromList
      [ (AssetId testPolicyId hydraHeadV2AssetName, -1)
      , (AssetId testPolicyId (UnsafeAssetName "swap"), 1)
      ]

-- | The healthy increment with its required signers stripped. Version and value are untouched (so the
-- old version+value-only reference accepted it), but no signer now holds a PT, so the new participant
-- conjunct rejects and the validator rejects too (@NoSigners@): the @mustBeSignedByParticipant@ gap the
-- old (mocked) reference missed.
noSignerIncrementTx :: (Tx, UTxO)
noSignerIncrementTx = applyMutation (ChangeRequiredSigners []) healthyIncrementTx

-- | The healthy increment made to mint an extra token. Version\/value\/signers are untouched (so the
-- old reference accepted it), but the new no-mint conjunct rejects and the validator rejects too
-- (@MintingOrBurningIsForbidden@): the @mustNotMintOrBurn@ gap the old (mocked) reference missed.
mintingIncrementTx :: (Tx, UTxO)
mintingIncrementTx = applyMutation (ChangeMintedValue minted) healthyIncrementTx
 where
  minted = IsList.fromList [(AssetId testPolicyId (UnsafeAssetName "mint"), 1)]

-- | The healthy decrement with the head output's STATE token swapped 1-for-1 for an unrelated token: the
-- non-ada TOTAL is unchanged (so 'decRefVerdict' still accepts), yet the per-asset conjunct sees the ST
-- short by one and the validator rejects (@mustDecreaseValue@). The decrement mirror of
-- 'balancedSwapIncrementTx'.
balancedSwapDecrementTx :: (Tx, UTxO)
balancedSwapDecrementTx =
  applyMutation (ChangeOutput 0 (modifyTxOutValue (<> tokenSwap) headOut)) healthyDecrementTx
 where
  headOut = fromJust (txOuts' (fst healthyDecrementTx) !!? 0)
  tokenSwap =
    IsList.fromList
      [ (AssetId testPolicyId hydraHeadV2AssetName, -1)
      , (AssetId testPolicyId (UnsafeAssetName "swap"), 1)
      ]

-- ── property assembly ─────────────────────────────────────────────────────────────────────────

-- | @reference-rejects ⇒ validator-rejects@; abstains (no constraint) when the reference can't
-- read the tx or accepts.
differential :: ((Tx, UTxO) -> Maybe Bool) -> (Tx, UTxO) -> Property
differential verdict m =
  case verdict m of
    Just False -> validatorAccepts m === False
    _ -> property True

mutated :: (Tx, UTxO) -> SomeMutation -> (Tx, UTxO)
mutated healthy SomeMutation{mutation} = applyMutation mutation healthy

familySpec ::
  String ->
  (Tx, UTxO) ->
  ((Tx, UTxO) -> Gen SomeMutation) ->
  ((Tx, UTxO) -> Maybe Bool) ->
  Spec
familySpec name healthy genMutation verdict = do
  prop (name <> ": reference accepts the healthy tx") $
    verdict healthy === Just True
  prop (name <> ": validator accepts the healthy tx") $
    validatorAccepts healthy === True
  prop (name <> ": reference-reject ⇒ validator-reject") $
    forAll (genMutation healthy) $ \sm ->
      differential verdict (mutated healthy sm)

spec :: Spec
spec = parallel $ do
  familySpec "increment" healthyIncrementTx genIncrementMutation incRefVerdict
  familySpec "decrement" healthyDecrementTx genDecrementMutation decRefVerdict
  familySpec "contest" healthyContestTx genContestMutation contestRefVerdict
  familySpec "fanout" healthyFanoutTx genFanoutMutation fanoutRefVerdict
  familySpec "partialFanout" healthyIntermediatePartialFanoutTx genPartialFanoutMutation partialFanoutRefVerdict

  -- Demonstration that un-mocking the non-ada value conservation closes a real gap: a pure
  -- native-token siphon passes the old lovelace-only check but the new non-ada conjunct catches it,
  -- and the validator rejects it too.
  prop "increment: a pure non-ada token siphon STILL passes the old lovelace-only check" $
    incLovelaceConserved tokenSiphonIncrementTx === Just True
  prop "increment: the new non-ada conjunct REJECTS the token siphon the lovelace check missed" $
    incRefVerdict tokenSiphonIncrementTx === Just False
  prop "increment: validator also rejects the non-ada token siphon" $
    validatorAccepts tokenSiphonIncrementTx === False

  -- Demonstration that un-mocking the participant signature (shared checker) closes a real gap:
  -- stripping the required signers leaves version+value conservation intact (so the old reference
  -- accepted it) but the new participant conjunct rejects, and the validator rejects too (NoSigners).
  prop "increment: dropping the participant signer leaves lovelace conservation intact" $
    incLovelaceConserved noSignerIncrementTx === Just True
  prop "increment: the participant conjunct REJECTS a tx with no participant signer" $
    incRefVerdict noSignerIncrementTx === Just False
  prop "increment: validator also rejects the tx with no participant signer" $
    validatorAccepts noSignerIncrementTx === False

  -- Demonstration that the PER-ASSET refinement closes a further gap the non-ada TOTAL misses: a balanced
  -- 1-for-1 token swap keeps the total constant (so the total-only reference still accepts it), but the
  -- per-asset conjunct catches it, and the validator rejects it (mustPreserveValue).
  prop "increment: per-asset accepts the healthy increment" $
    incPerAssetVerdict healthyIncrementTx === Just True
  prop "increment: a balanced token swap STILL passes the non-ada TOTAL check" $
    incRefVerdict balancedSwapIncrementTx === Just True
  prop "increment: the per-asset conjunct REJECTS the balanced swap the total check missed" $
    incPerAssetVerdict balancedSwapIncrementTx === Just False
  prop "increment: validator also rejects the balanced token swap" $
    validatorAccepts balancedSwapIncrementTx === False

  -- Demonstration that un-mocking the no-mint conjunct closes a real gap: a minting mutation leaves
  -- version/value/signers intact (so the old reference accepted it) but the new no-mint conjunct rejects,
  -- and the validator rejects too (MintingOrBurningIsForbidden).
  prop "increment: the no-mint conjunct REJECTS a tx that mints" $
    incRefVerdict mintingIncrementTx === Just False
  prop "increment: validator also rejects the minting tx" $
    validatorAccepts mintingIncrementTx === False

  -- The depositSpent conjunct (the redeemer's claimed deposit ref must be among the spent inputs) is
  -- exercised by genIncrementMutation's invalid-deposit-ref case in the family test above.

  -- Decrement per-asset, mirroring the increment per-asset gap: a balanced 1-for-1 swap keeps the non-ada
  -- TOTAL constant (so the total-only decrement reference still accepts) but the per-asset conjunct catches
  -- it, and the validator rejects (mustDecreaseValue).
  prop "decrement: per-asset accepts the healthy decrement" $
    decPerAssetVerdict healthyDecrementTx === Just True
  prop "decrement: a balanced token swap STILL passes the non-ada TOTAL check" $
    decRefVerdict balancedSwapDecrementTx === Just True
  prop "decrement: the per-asset conjunct REJECTS the balanced swap the total check missed" $
    decPerAssetVerdict balancedSwapDecrementTx === Just False
  prop "decrement: validator also rejects the balanced token swap" $
    validatorAccepts balancedSwapDecrementTx === False
