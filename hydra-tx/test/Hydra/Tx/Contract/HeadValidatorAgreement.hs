{-# LANGUAGE OverloadedStrings #-}

-- | Function-level agreement between the Agda-extracted reference checker and the REAL Plutus
-- validator, with NO transactions, NO ledger evaluation, and NO mutation generators.
--
-- The validator is a plain Haskell function (@Head.headValidator :: ScriptHash -> State -> Input ->
-- ScriptContext -> Bool@). So we construct its inputs directly, run BOTH the validator and the Agda
-- reference (@Ref.checkClose@) on the SAME inputs, and assert @reference === validator@. The fields the
-- reference models are generated independently (so e.g. the produced version is sometimes equal to the
-- input version, sometimes not), which exercises BOTH the accept and the reject directions in one
-- property — unlike the old approach, which reused the hydra mutation generators (a corpus that is
-- validator-rejecting by construction, making @reference-reject ⇒ validator-reject@ vacuous).
--
-- Crypto is satisfied by construction: this spike covers @CloseInitial@, the one close case with no
-- signature (it requires only the empty-accumulator BLS G1 generator), so no signing is needed.
--
-- This is the close/CloseInitial spike that validates the whole approach; the other families/redeemers
-- follow the same pattern.
module Hydra.Tx.Contract.HeadValidatorAgreement (spec) where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  Ed25519DSIGN,
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (blsCompress)
import Cardano.Crypto.Hash (SHA256, digest)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Plutus.CostModels (getCostModelParams)
import Control.Monad.Writer (runWriterT)
import Data.ByteString qualified as BS
import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (pattern PlutusScriptSerialised)
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as HS
import Hydra.Contract.HeadTokens qualified as Tokens
import Hydra.Contract.Util (hashTxOuts, hydraHeadV2)
import Hydra.Data.ContestationPeriod (ContestationPeriod (..))
import Hydra.Data.Party (Party, partyFromVerificationKeyBytes)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Tx.Accumulator qualified as Accumulator
import PlutusLedgerApi.V1.Value (adaSymbol, adaToken, singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  CurrencySymbol (..),
  Datum (..),
  EvaluationContext,
  Extended (..),
  Interval (..),
  LowerBound (..),
  MajorProtocolVersion (..),
  OutputDatum (..),
  POSIXTime (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptForEvaluation,
  ScriptHash (..),
  ScriptInfo (..),
  ScriptPurpose (..),
  SerialisedScript,
  TokenName (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  UpperBound (..),
  Value,
  VerboseMode (..),
  deserialiseScript,
  emptyMintValue,
  evaluateScriptCounting,
  mkEvaluationContext,
  toData,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (..))
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins qualified as Builtins
import Test.Hydra.Ledger.Cardano.Fixtures (plutusV3CostModel)
import Test.Hydra.Prelude
import Test.QuickCheck (choose, elements, forAll, (.&&.), (===))

-- ── fixed, well-formed scaffolding (held healthy; only the modeled fields below are varied) ──────────

headPolicy :: CurrencySymbol
headPolicy = CurrencySymbol "00000000000000000000000000000000000000000000000000000000"

headScriptHash :: ScriptHash
headScriptHash = ScriptHash "11111111111111111111111111111111111111111111111111111111"

headAddr :: Address
headAddr = Address (ScriptCredential headScriptHash) Nothing

ownRef :: TxOutRef
ownRef = TxOutRef (TxId "22222222222222222222222222222222222222222222222222222222222222222222") 0

-- A single signer whose key-hash IS a participation-token name in the head value (so
-- mustBeSignedByParticipant holds).
signerKH :: PubKeyHash
signerKH = PubKeyHash "33333333333333333333333333333333333333333333333333333333"

ptName :: TokenName
ptName = TokenName (getPubKeyHash signerKH)

-- Head value carries the participation token; identical on input and output (mustPreserveHeadValue).
headVal :: Value
headVal = singleton adaSymbol adaToken 2_000_000 <> singleton headPolicy ptName 1

-- The empty-accumulator KZG commitment is the BLS G1 generator (isG1Generator).
g1Generator :: Builtins.BuiltinBLS12_381_G1_Element
g1Generator = Builtins.bls12_381_G1_uncompress Builtins.bls12_381_G1_compressed_generator

-- Open input version is fixed to 0 (CloseInitial requires version == 0); the OPEN contestation period
-- (ms) is fixed; the lower validity bound is fixed. The reject direction comes from varying the produced
-- (Closed) fields, not from a "healthy base + mutation".
openVersionN :: Integer
openVersionN = 0

openCpMs :: Integer
openCpMs = 100

validityLoN :: Integer
validityLoN = 1_000

openDatum :: HS.OpenDatum
openDatum =
  HS.OpenDatum
    { HS.headSeed = ownRef
    , HS.headId = headPolicy
    , HS.parties = []
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = openVersionN
    , HS.accumulatorHash = Builtins.toBuiltin ("" :: ByteString)
    , HS.headAdaOverhead = 0
    }

-- ── the constructed inputs, parameterized over the fields the reference models ────────────────────────

closedDatum :: Integer -> Integer -> Integer -> Integer -> Integer -> HS.ClosedDatum
closedDatum closedVersion closedCpMs closedSnap contestersLen deadline =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = []
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger closedCpMs)
    , HS.version = closedVersion
    , HS.snapshotNumber = closedSnap
    , HS.contesters = replicate (fromInteger contestersLen) signerKH
    , HS.contestationDeadline = POSIXTime deadline
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = 0
    }

mkContext :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ScriptContext
mkContext closedVersion closedCpMs closedSnap contestersLen deadline tMax =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headInputOut]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOutputOut]
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange =
              Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime tMax)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Close HS.CloseInitial))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open openDatum))))
    }
 where
  headInputOut = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open openDatum)))) Nothing
  headOutputOut =
    TxOut
      headAddr
      headVal
      (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (closedDatum closedVersion closedCpMs closedSnap contestersLen deadline)))))
      Nothing

-- ── the two oracles on the SAME inputs ───────────────────────────────────────────────────────────────

-- The real Plutus validator (a plain Haskell function). The leading ScriptHash (the CRS) is unused for
-- close.
validatorVerdict :: ScriptContext -> Bool
validatorVerdict = Head.headValidator headScriptHash (HS.Open openDatum) (HS.Close HS.CloseInitial)

-- The Agda-extracted reference on the SAME modeled fields.
referenceVerdict :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
referenceVerdict closedVersion closedCpMs closedSnap contestersLen deadline tMax =
  Ref.checkClose
    (Ref.mkOps (\_ _ _ -> True))
    (Ref.MkOpen openVersionN openCpMs)
    (Ref.MkClosed closedVersion closedCpMs closedSnap contestersLen deadline)
    Ref.CloseInitialT
    tMax
    validityLoN

-- ── signed CloseUnused: a REAL Ed25519 signature over the exact validator message ──────────────────────
-- CloseInitial needs no signature. CloseUnused does: the validator runs `verifySnapshotSignature` for real
-- (it is NOT mocked on the validator side, only on the reference side). We hold the test key, so we produce
-- a genuine signature the real validator accepts — and a deliberately wrong one it must reject.

-- Our own snapshot signing key (deterministic; the "mocked" crypto is just key material we control).
snapshotSK :: SignKeyDSIGN Ed25519DSIGN
snapshotSK = genKeyDSIGN (mkSeedFromBytes (digest (Proxy :: Proxy SHA256) ("hva-snapshot-seed" :: ByteString)))

snapshotParty :: Party
snapshotParty = partyFromVerificationKeyBytes (rawSerialiseVerKeyDSIGN (deriveVerKeyDSIGN snapshotSK))

-- Empty-set accumulator hash, matching mustMatchAccumulatorCommitmentHash (blake2b_256 ∘ compress).
emptyAccHash :: Builtins.BuiltinByteString
emptyAccHash = Builtins.blake2b_256 (Builtins.bls12_381_G1_compress g1Generator)

-- The exact bytes the validator reconstructs for CloseUnused: serialiseData of (headId, OPEN version,
-- snapshotNumber', accumulatorHash). Signing this with snapshotSK makes verifySnapshotSignature accept.
closeMsg :: Integer -> ByteString
closeMsg snap =
  Builtins.fromBuiltin $
    Builtins.serialiseData (PlutusTx.toBuiltinData headPolicy)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData openVersionN)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData snap)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData emptyAccHash)

closeSigFor :: Integer -> HS.Signature
closeSigFor snap = Builtins.toBuiltin (rawSerialiseSigDSIGN (signDSIGN () (closeMsg snap) snapshotSK))

-- CloseUnused datums carry the snapshot signer as the head party (so verifySnapshotSignature + the
-- mustNotChangeParameters parties-preservation both hold). Built explicitly (not by record update) because
-- `parties` is a duplicate field across the datum records, so a single-field update is ambiguous.
openDatumU :: HS.OpenDatum
openDatumU =
  HS.OpenDatum
    { HS.headSeed = ownRef
    , HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = openVersionN
    , HS.accumulatorHash = Builtins.toBuiltin ("" :: ByteString)
    , HS.headAdaOverhead = 0
    }

closedDatumU :: Integer -> Integer -> Integer -> Integer -> Integer -> HS.ClosedDatum
closedDatumU cv ccp cs cl dl =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger ccp)
    , HS.version = cv
    , HS.snapshotNumber = cs
    , HS.contesters = replicate (fromInteger cl) signerKH
    , HS.contestationDeadline = POSIXTime dl
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = 0
    }

-- Build the CloseUnused context for a given redeemer (so we can supply a valid OR a bad signature).
mkContextU :: HS.CloseRedeemer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ScriptContext
mkContextU redeemer cv ccp cs cl dl tMax =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headInU]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOutU]
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange =
              Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime tMax)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Close redeemer))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open openDatumU))))
    }
 where
  headInU = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open openDatumU)))) Nothing
  headOutU = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (closedDatumU cv ccp cs cl dl))))) Nothing

-- The CloseUnused redeemer with a VALID signature over the given snapshot number.
unusedRedeemer :: Integer -> HS.CloseRedeemer
unusedRedeemer cs = HS.CloseUnused{HS.signature = [closeSigFor cs], HS.accumulatorHash = emptyAccHash}

-- both oracles on a valid-signature CloseUnused (decidable-conjunct agreement: crypto valid on both sides).
unusedRef :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
unusedRef cv ccp cs cl dl tMax =
  Ref.checkClose
    (Ref.mkOps (\_ _ _ -> True))
    (Ref.MkOpen openVersionN openCpMs)
    (Ref.MkClosed cv ccp cs cl dl)
    Ref.CloseUnusedT
    tMax
    validityLoN

unusedVal :: HS.CloseRedeemer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
unusedVal redeemer cv ccp cs cl dl tMax =
  Head.headValidator headScriptHash (HS.Open openDatumU) (HS.Close redeemer) (mkContextU redeemer cv ccp cs cl dl tMax)

-- ── increment (Open→Open: version bump + value flow + a deposit script input + signature) ───────────────
-- checkIncrement finds the head input by its STATE token (hasST), requires headIn ◇ Σdeposits == headOut,
-- bumps the version, and verifies a signature over (headId, prevVersion, snapshotNumber, nextAccumulatorHash).
-- We hold everything healthy except the two MODELED conjuncts (version bump, value) and assert
-- reference (Ref.checkInc) === validator; plus a crypto non-vacuity (bad sig → validator rejects).

stName :: TokenName
stName = TokenName hydraHeadV2

-- the head value carries the ST (so hasST finds the head input) AND a PT (mustBeSignedByParticipant).
incHeadVal :: Value
incHeadVal = singleton adaSymbol adaToken 2_000_000 <> singleton headPolicy stName 1 <> singleton headPolicy ptName 1

depRef :: TxOutRef
depRef = TxOutRef (TxId "55555555555555555555555555555555555555555555555555555555555555555555") 0

depAddr :: Address
depAddr = Address (ScriptCredential (ScriptHash "66666666666666666666666666666666666666666666666666666666")) Nothing

depVal :: Value
depVal = singleton adaSymbol adaToken 500_000

incNextAccHash :: Builtins.BuiltinByteString
incNextAccHash = Builtins.toBuiltin ("inc-acc" :: ByteString)

-- identical to openDatumU (open datum with the snapshot signer as sole party).
incOpenPrev :: HS.OpenDatum
incOpenPrev = openDatumU

incOpenNext :: Integer -> HS.OpenDatum
incOpenNext nextV = incOpenPrev{HS.version = nextV, HS.accumulatorHash = incNextAccHash}

-- message the validator reconstructs: (headId, OPEN version, snapshotNumber, nextAccumulatorHash).
incMsg :: Integer -> ByteString
incMsg snap =
  Builtins.fromBuiltin $
    Builtins.serialiseData (PlutusTx.toBuiltinData headPolicy)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData openVersionN)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData snap)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData incNextAccHash)

incSigFor :: Integer -> HS.Signature
incSigFor snap = Builtins.toBuiltin (rawSerialiseSigDSIGN (signDSIGN () (incMsg snap) snapshotSK))

incRedeemer :: Integer -> HS.IncrementRedeemer
incRedeemer snap = HS.IncrementRedeemer{HS.signature = [incSigFor snap], HS.snapshotNumber = snap, HS.increment = depRef}

-- build the increment context. `nextV` is the produced version; `vPerturb` adds extra ada to the head
-- output (breaking value conservation when ≠ 0). Everything else is healthy.
mkIncContext :: HS.IncrementRedeemer -> Integer -> Integer -> ScriptContext
mkIncContext redeemer nextV vPerturb =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headIn, TxInInfo depRef depIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOut]
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime 2_000)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Increment redeemer))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev))))
    }
 where
  headIn = TxOut headAddr incHeadVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev)))) Nothing
  depIn = TxOut depAddr depVal (OutputDatum (Datum (PlutusTx.toBuiltinData (0 :: Integer)))) Nothing
  headOut =
    TxOut
      headAddr
      (incHeadVal <> depVal <> singleton adaSymbol adaToken vPerturb)
      (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open (incOpenNext nextV)))))
      Nothing

-- reference: version bump + value conservation (ada + non-ada totals). The healthy increment adds
-- 500_000 ada and no tokens; ST+PT give non-ada total 2 on both head input and output.
incRef :: Integer -> Integer -> Bool
incRef nextV vPerturb =
  Ref.checkInc
    (Ref.mkOpsInc (const True))
    (Ref.MkIncIO openVersionN nextV 2_000_000 500_000 (2_500_000 + vPerturb) 2 0 2)

incVal :: HS.IncrementRedeemer -> Integer -> Integer -> Bool
incVal redeemer nextV vPerturb =
  Head.headValidator headScriptHash (HS.Open incOpenPrev) (HS.Increment redeemer) (mkIncContext redeemer nextV vPerturb)

-- ── decrement (Open→Open: version bump + value SHRINKS by decommit OUTPUTS + signature) ─────────────────
-- checkDecrement finds the head input via findOwnInput and requires headIn == headOut ◇ Σdecommit-outputs
-- (the decommitted value leaves via tx outputs [1..m]). Same signature format as increment.

decHeadInVal :: Value
decHeadInVal = singleton adaSymbol adaToken 2_500_000 <> singleton headPolicy stName 1 <> singleton headPolicy ptName 1

decRedeemer :: Integer -> HS.DecrementRedeemer
decRedeemer snap = HS.DecrementRedeemer{HS.signature = [incSigFor snap], HS.snapshotNumber = snap, HS.numberOfDecommitOutputs = 1}

-- `vPerturb` adds extra ada to the head output (breaking value decrease when ≠ 0). Decommit = 500_000 ada.
mkDecContext :: HS.DecrementRedeemer -> Integer -> Integer -> ScriptContext
mkDecContext redeemer nextV vPerturb =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOut, decommitOut]
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime 2_000)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Decrement redeemer))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev))))
    }
 where
  headIn = TxOut headAddr decHeadInVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev)))) Nothing
  headOut =
    TxOut
      headAddr
      (singleton adaSymbol adaToken (2_000_000 + vPerturb) <> singleton headPolicy stName 1 <> singleton headPolicy ptName 1)
      (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open (incOpenNext nextV)))))
      Nothing
  decommitOut = TxOut (Address (PubKeyCredential signerKH) Nothing) (singleton adaSymbol adaToken 500_000) NoOutputDatum Nothing

decRef :: Integer -> Integer -> Bool
decRef nextV vPerturb =
  Ref.checkDec
    (Ref.mkOpsInc (const True))
    (Ref.MkIncIO openVersionN nextV 2_500_000 500_000 (2_000_000 + vPerturb) 2 0 2)

decVal :: HS.DecrementRedeemer -> Integer -> Integer -> Bool
decVal redeemer nextV vPerturb =
  Head.headValidator headScriptHash (HS.Open incOpenPrev) (HS.Decrement redeemer) (mkDecContext redeemer nextV vPerturb)

-- ── contest (Closed→Closed: version preserved + snapshot increases + one contester + deadline + sig) ────
-- One party (= one contester), so mustPushDeadline keeps the deadline (contesters'==parties'). The contester
-- is the lone signatory (signerKH); the snapshot signature is over (headId, version, snapshotNumber', accHash).
contestPrev :: HS.ClosedDatum
contestPrev =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = 0
    , HS.snapshotNumber = 0
    , HS.contesters = []
    , HS.contestationDeadline = POSIXTime 2_000
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = 0
    }

contestNext :: Integer -> Integer -> HS.ClosedDatum
contestNext sPrime tfinPerturb =
  contestPrev{HS.snapshotNumber = sPrime, HS.contesters = [signerKH], HS.contestationDeadline = POSIXTime (2_000 + tfinPerturb)}

contestMsg :: Integer -> ByteString
contestMsg sPrime =
  Builtins.fromBuiltin $
    Builtins.serialiseData (PlutusTx.toBuiltinData headPolicy)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData (0 :: Integer))
      <> Builtins.serialiseData (PlutusTx.toBuiltinData sPrime)
      <> Builtins.serialiseData (PlutusTx.toBuiltinData emptyAccHash)

contestSigFor :: Integer -> HS.Signature
contestSigFor sPrime = Builtins.toBuiltin (rawSerialiseSigDSIGN (signDSIGN () (contestMsg sPrime) snapshotSK))

contestRedeemer :: Integer -> HS.ContestRedeemer
contestRedeemer sPrime = HS.ContestUnused{HS.signature = [contestSigFor sPrime], HS.accumulatorHash = emptyAccHash}

mkContestContext :: HS.ContestRedeemer -> Integer -> Integer -> Integer -> ScriptContext
mkContestContext redeemer sPrime tfinPerturb tMax =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOut]
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime tMax)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Contest redeemer))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Closed contestPrev))))
    }
 where
  headIn = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed contestPrev)))) Nothing
  headOut = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (contestNext sPrime tfinPerturb))))) Nothing

contestRef :: Integer -> Integer -> Integer -> Bool
contestRef sPrime tfinPerturb tMax =
  Ref.checkContest
    (Ref.mkOpsContest (const True))
    (Ref.MkContestIO 0 0 0 sPrime 0 1 2_000 tMax (2_000 + tfinPerturb) 1 openCpMs)

contestVal :: HS.ContestRedeemer -> Integer -> Integer -> Integer -> Bool
contestVal redeemer sPrime tfinPerturb tMax =
  Head.headValidator headScriptHash (HS.Closed contestPrev) (HS.Contest redeemer) (mkContestContext redeemer sPrime tfinPerturb tMax)

-- ── init (μHead minting policy: token COUNT + PLACEMENT) ────────────────────────────────────────────────
-- validateTokensMinting checks: the head policy MINTS exactly n+1 tokens (checkNumberOfTokens), the head
-- output carries the single ST (singleSTIsPaidToTheHead) and exactly n unique PTs
-- (enoughUniquePTsPaidToHead), plus the seed input is consumed and the datum binds headId/seed. We hold the
-- seed + datum healthy and vary the three modeled quantities (minted count, ST quantity, PT count) to
-- exercise the accept AND reject directions, asserting Ref.checkInit === validateTokensMinting. n = 1 party.

initSeedRef :: TxOutRef
initSeedRef = TxOutRef (TxId "77777777777777777777777777777777777777777777777777777777777777777777") 0

-- one party (snapshotParty); headId = the minting currency; headSeed = the consumed seed (datum binding).
initOpenDatum :: HS.OpenDatum
initOpenDatum = openDatum{HS.headSeed = initSeedRef, HS.parties = [snapshotParty]}

-- distinct 1-byte PT names, none equal to the 11-byte ST name (hydraHeadV2).
initPtNames :: [TokenName]
initPtNames =
  [ TokenName (Builtins.toBuiltin ("\1" :: ByteString))
  , TokenName (Builtins.toBuiltin ("\2" :: ByteString))
  , TokenName (Builtins.toBuiltin ("\3" :: ByteString))
  ]

-- head output value: ada + ST(stQty) + numPT PTs (each qty 1). headTokenCount (sum) = stQty + numPT.
initHeadVal :: Integer -> Integer -> Value
initHeadVal stQty numPT =
  singleton adaSymbol adaToken 2_000_000
    <> (if stQty == 0 then mempty else singleton headPolicy stName stQty)
    <> mconcat [singleton headPolicy nm 1 | nm <- take (fromInteger numPT) initPtNames]

initMint :: Integer -> MintValue
initMint mintedCount = UnsafeMintValue (AMap.unsafeFromList [(headPolicy, AMap.unsafeFromList [(stName, mintedCount)])])

-- `inputSeedRef` is the out-ref of the (only) tx input; the validator's seedInput arg is fixed to
-- initSeedRef, so passing a different inputSeedRef breaks seedInputIsConsumed (a validator-only conjunct).
mkInitContext :: TxOutRef -> Integer -> Integer -> Integer -> ScriptContext
mkInitContext inputSeedRef mintedCount stQty numPT =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo inputSeedRef seedIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOut]
          , txInfoFee = 0
          , txInfoMint = initMint mintedCount
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime 2_000)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData ())
    , scriptContextScriptInfo = MintingScript headPolicy
    }
 where
  seedIn = TxOut (Address (PubKeyCredential signerKH) Nothing) (singleton adaSymbol adaToken 10_000_000) NoOutputDatum Nothing
  headOut = TxOut headAddr (initHeadVal stQty numPT) (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open initOpenDatum)))) Nothing

initRef :: Integer -> Integer -> Integer -> Bool
initRef mintedCount stQty numPT =
  Ref.checkInit (Ref.mkOpsInit (const True)) (Ref.MkMintIO 1 mintedCount stQty (stQty + numPT))

initVal :: Integer -> Integer -> Integer -> Bool
initVal mintedCount stQty numPT =
  Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext initSeedRef mintedCount stQty numPT)

-- ── νDeposit (recover/claim): the REAL Aiken validator, run as compiled UPLC on a hand-built context ────
-- The deposit validator is Aiken (deposit.ak), not a Haskell function, so we cannot call it directly. We
-- deserialise the compiled validator from plutus.json, build a V3 EvaluationContext from the test cost
-- model, construct a ScriptContext directly, serialise it, and run the CEK machine on it (no transaction,
-- no fixtures, no mutations). A successful evaluation = accept; any script error = reject. We then assert
-- Ref.checkRecover / Ref.checkClaim === depositAccepts over independently-generated deadlines and head ids.

-- The V3 evaluation context built from the test PlutusV3 cost model (the same model the ledger fixtures use).
depositEvalContext :: EvaluationContext
depositEvalContext =
  case runWriterT (mkEvaluationContext (getCostModelParams plutusV3CostModel)) of
    Left err -> error ("deposit cost model: " <> show err)
    Right (ec, _warns) -> ec

-- The compiled deposit validator, deserialised ready for evaluation (major protocol version 11, the value
-- the test pparams pin).
depositScriptForEval :: ScriptForEvaluation
depositScriptForEval =
  case deserialiseScript (MajorProtocolVersion 11) serialised of
    Left err -> error ("deposit deserialise: " <> show err)
    Right s -> s
 where
  serialised :: SerialisedScript
  serialised = case depositValidatorScript of PlutusScriptSerialised sbs -> sbs

-- Run the actual compiled deposit validator on a directly-constructed V3 ScriptContext.
depositAccepts :: ScriptContext -> Bool
depositAccepts ctx =
  case snd (evaluateScriptCounting (MajorProtocolVersion 11) Quiet depositEvalContext depositScriptForEval (toData ctx)) of
    Right _ -> True
    Left _ -> False

-- A deposit script address (the deposit input being spent) and the deposit out-ref.
depositScriptAddr :: Address
depositScriptAddr = Address (ScriptCredential (ScriptHash "99999999999999999999999999999999999999999999999999999999")) Nothing

depositOwnRef :: TxOutRef
depositOwnRef = TxOutRef (TxId "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") 0

-- Deposit datum: head id, deadline, EMPTY commit list. The empty list makes recover_outputs trivially
-- satisfied (take 0 outputs == [] and sort [] == [], so both sha2_256 hashes are over the empty string),
-- isolating the modeled deadline conjunct (the recovered-outputs hash equality is the mocked Ops boundary).
depositDatum :: CurrencySymbol -> Integer -> Datum
depositDatum headCid deadline = Deposit.datum ((headCid, POSIXTime deadline, []) :: Deposit.DepositDatum)

-- ── Recover (posted strictly AFTER the recover deadline: validityLo > deadline) ──
mkRecoverContext :: Integer -> Integer -> ScriptContext
mkRecoverContext deadline validityLo =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo depositOwnRef depIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = []
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLo)) True) (UpperBound PosInf True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Deposit.redeemer (Deposit.Recover 0)
    , scriptContextScriptInfo = SpendingScript depositOwnRef (Just (depositDatum headPolicy deadline))
    }
 where
  depIn = TxOut depositScriptAddr (singleton adaSymbol adaToken 3_000_000) (OutputDatum (depositDatum headPolicy deadline)) Nothing

recoverRef :: Integer -> Integer -> Bool
recoverRef deadline validityLo =
  Ref.checkRecover (Ref.mkOpsRecover (const True)) (Ref.MkRecoverIO deadline validityLo)

recoverVal :: Integer -> Integer -> Bool
recoverVal deadline validityLo = depositAccepts (mkRecoverContext deadline validityLo)

-- ── Claim (posted BEFORE the deadline: validityHi <= deadline, AND the head input carries the deposit's
-- head id ST and is spent by an Increment) ──
claimHeadInRef :: TxOutRef
claimHeadInRef = TxOutRef (TxId "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb") 0

-- the redeemer spending the head input: an Increment (constr index 0), satisfying is_head_increment.
claimHeadRedeemer :: Redeemer
claimHeadRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Increment (incRedeemer 0)))

-- `depHeadCid` is the deposit datum's head id; the head input always carries headPolicy's ST. When they
-- differ, expect_increment_redeemer finds no matching head input and the validator rejects (own-head bind).
mkClaimContext :: Integer -> Integer -> CurrencySymbol -> ScriptContext
mkClaimContext deadline validityHi depHeadCid =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo depositOwnRef depIn, TxInInfo claimHeadInRef headIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = []
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound NegInf True) (UpperBound (Finite (POSIXTime validityHi)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.unsafeFromList [(Spending claimHeadInRef, claimHeadRedeemer)]
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Deposit.redeemer Deposit.Claim
    , scriptContextScriptInfo = SpendingScript depositOwnRef (Just (depositDatum depHeadCid deadline))
    }
 where
  depIn = TxOut depositScriptAddr (singleton adaSymbol adaToken 3_000_000) (OutputDatum (depositDatum depHeadCid deadline)) Nothing
  headIn = TxOut headAddr (singleton adaSymbol adaToken 5_000_000 <> singleton headPolicy stName 1) (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev)))) Nothing

-- Deterministic encoding of a head-id currency symbol as the big-endian integer of its bytes: equal iff the
-- symbols are equal (the cid-binding check needs nothing more — see the cidToNat note in ReferenceBridge).
cidToInteger :: CurrencySymbol -> Integer
cidToInteger = BS.foldl' (\acc w -> acc * 256 + toInteger w) 0 . Builtins.fromBuiltin . unCurrencySymbol

claimRef :: Integer -> Integer -> CurrencySymbol -> Bool
claimRef deadline validityHi depHeadCid =
  Ref.checkClaim (Ref.mkOpsClaim (const True)) (Ref.MkClaimIO deadline validityHi (cidToInteger depHeadCid) (cidToInteger headPolicy))

claimVal :: Integer -> Integer -> CurrencySymbol -> Bool
claimVal deadline validityHi depHeadCid = depositAccepts (mkClaimContext deadline validityHi depHeadCid)

-- a head id distinct from headPolicy, for exercising the own-head-binding reject direction.
otherHeadCid :: CurrencySymbol
otherHeadCid = CurrencySymbol "abababababababababababababababababababababababababababab"

-- ── full fanout (Closed → finalised, empty head: m = 0, the only path that burns the head tokens) ───────
-- headIsFinalizedWith checks: all n+1 head tokens burned (mustBurnAllHeadTokens), posted after the
-- contestation deadline (validityLo > tfinal), the distributed outputs are accumulator members
-- (checkCRSAndMembership), and value is conserved (mustConserveValue). For the empty head m = 0, so the
-- subset is empty and checkMembershipPairing reduces to commitment == proof: with the empty-accumulator
-- G1 generator as both the commitment and the proof, and a CRS reference input carrying [g2Generator], the
-- REAL BLS pairing check runs and passes. We vary the burned-token count and the lower validity bound to
-- exercise both directions and assert Ref.checkFanout === headIsFinalizedWith. n = 1.

g2Generator :: Builtins.BuiltinBLS12_381_G2_Element
g2Generator = Builtins.bls12_381_G2_uncompress Builtins.bls12_381_G2_compressed_generator

-- The CRS reference script hash (the leading argument to the head validator) and the reference input
-- carrying the trusted-setup G2 points. For the empty subset only the first point ([g2Generator]) is used.
crsScriptHash :: ScriptHash
crsScriptHash = ScriptHash "cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

crsRefOut :: TxOutRef
crsRefOut = TxOutRef (TxId "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd") 0

crsRefInput :: TxInInfo
crsRefInput =
  TxInInfo
    crsRefOut
    ( TxOut
        (Address (ScriptCredential crsScriptHash) Nothing)
        (singleton adaSymbol adaToken 2_000_000)
        (OutputDatum (Datum (PlutusTx.toBuiltinData [g2Generator])))
        (Just crsScriptHash)
    )

fanoutOverhead :: Integer
fanoutOverhead = 2_000_000

-- the head input value: the n+1 = 2 head tokens (ST + PT) plus the locked ada overhead.
fanoutHeadInVal :: Value
fanoutHeadInVal = singleton headPolicy stName 1 <> singleton headPolicy ptName 1 <> singleton adaSymbol adaToken fanoutOverhead

-- Closed datum before the empty-head fanout: empty-accumulator commitment (g1Generator), one party.
fanoutClosedDatum :: Integer -> HS.ClosedDatum
fanoutClosedDatum tfinal =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = 0
    , HS.snapshotNumber = 0
    , HS.contesters = []
    , HS.contestationDeadline = POSIXTime tfinal
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = fanoutOverhead
    }

-- token names available to burn; the healthy burn is ST + PT (n+1 = 2). Burning a different count breaks
-- both mustBurnAllHeadTokens and mustConserveValue (the head input fixes the conserved value at ST+PT+ada).
fanoutBurnNames :: [TokenName]
fanoutBurnNames = [stName, ptName, TokenName (Builtins.toBuiltin ("\7" :: ByteString))]

fanoutMint :: Integer -> MintValue
fanoutMint burnedCount =
  UnsafeMintValue (AMap.unsafeFromList [(headPolicy, AMap.unsafeFromList [(nm, -1) | nm <- take (fromInteger burnedCount) fanoutBurnNames])])

fanoutRedeemer :: HS.Input
fanoutRedeemer = HS.Fanout{HS.numberOfFanoutOutputs = 0, HS.proof = g1Generator, HS.crsRef = crsRefOut}

mkFanoutContext :: Integer -> Integer -> Integer -> ScriptContext
mkFanoutContext burnedCount validityLo tfinal =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headIn]
          , txInfoReferenceInputs = [crsRefInput]
          , txInfoOutputs = []
          , txInfoFee = 0
          , txInfoMint = fanoutMint burnedCount
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLo)) True) (UpperBound PosInf True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData fanoutRedeemer)
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Closed (fanoutClosedDatum tfinal)))))
    }
 where
  headIn = TxOut headAddr fanoutHeadInVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (fanoutClosedDatum tfinal))))) Nothing

fanoutRef :: Integer -> Integer -> Integer -> Bool
fanoutRef burnedCount validityLo tfinal =
  Ref.checkFanout (Ref.mkOpsFanout (const True)) (Ref.MkFanout 0 burnedCount 1 tfinal validityLo)

fanoutVal :: Integer -> Integer -> Integer -> Bool
fanoutVal burnedCount validityLo tfinal =
  Head.headValidator crsScriptHash (HS.Closed (fanoutClosedDatum tfinal)) fanoutRedeemer (mkFanoutContext burnedCount validityLo tfinal)

-- ── partial fanout (Closed → FanoutProgress: distribute a subset, continue with the remaining acc) ──────
-- checkPartialFanout requires m > 0 distributed outputs (mustHaveOutputs), no mint, after the deadline, the
-- continuing FanoutProgress datum preserves the head parameters, value is conserved, and the membership
-- pairing e(oldAcc, G2) == e(newAcc, P_S(τ)·G2) holds. We build a REAL 2-element accumulator and distribute
-- one element: the accumulator is built directly over the on-chain element pre-image (hashTxOuts of the
-- distributed Plutus TxOut) — blake2b_224 is applied identically by the off-chain addElement and the
-- on-chain txOutsToSubsetScalars — so the proof (= the remaining accumulator's commitment) verifies against
-- the real CRS G2 powers of tau. We vary m (0 vs 1) and the lower validity bound and assert
-- Ref.checkPartialFanout === checkPartialFanout. n = 1.

-- the single distributed output and its on-chain element pre-image (sha2_256 of the serialised TxOut).
pfDistributedOut :: TxOut
pfDistributedOut = TxOut (Address (PubKeyCredential signerKH) Nothing) (singleton adaSymbol adaToken 1_500_000) NoOutputDatum Nothing

pfDistributedElem :: ByteString
pfDistributedElem = Builtins.fromBuiltin (hashTxOuts [pfDistributedOut])

-- a second accumulator element that stays in the head (not fanned out this batch); any distinct bytes.
pfRemainingElem :: ByteString
pfRemainingElem = "partial-fanout-remaining-element-marker"

pfFullAcc :: Accumulator.HydraAccumulator
pfFullAcc = Accumulator.build [pfDistributedElem, pfRemainingElem]

pfRemainingAcc :: Accumulator.HydraAccumulator
pfRemainingAcc = Accumulator.build [pfRemainingElem]

-- input accumulator commitment (over both elements) and the proof = remaining commitment (over one).
pfInputAccCommitment :: Builtins.BuiltinBLS12_381_G1_Element
pfInputAccCommitment = Accumulator.getAccumulatorCommitment pfFullAcc

pfNewAccCommitment :: Builtins.BuiltinBLS12_381_G1_Element
pfNewAccCommitment = Accumulator.getAccumulatorCommitment pfRemainingAcc

-- the on-chain CRS: the real trusted-setup G2 powers of tau (4 ≥ subset degree 1 + 1), matching the G1
-- setup the off-chain commitments were built against.
pfCrsData :: [Builtins.BuiltinBLS12_381_G2_Element]
pfCrsData = Builtins.bls12_381_G2_uncompress . Builtins.toBuiltin . blsCompress <$> Accumulator.crsG2Points 4

pfCrsRefInput :: TxInInfo
pfCrsRefInput =
  TxInInfo
    crsRefOut
    ( TxOut
        (Address (ScriptCredential crsScriptHash) Nothing)
        (singleton adaSymbol adaToken 2_000_000)
        (OutputDatum (Datum (PlutusTx.toBuiltinData pfCrsData)))
        (Just crsScriptHash)
    )

-- closed input carrying the full-accumulator commitment (built explicitly; accumulatorCommitment is a
-- duplicate field, so a record update would be ambiguous).
pfClosedDatum :: Integer -> HS.ClosedDatum
pfClosedDatum tfinal =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = 0
    , HS.snapshotNumber = 0
    , HS.contesters = []
    , HS.contestationDeadline = POSIXTime tfinal
    , HS.accumulatorCommitment = pfInputAccCommitment
    , HS.headAdaOverhead = fanoutOverhead
    }

-- the continuing FanoutProgress output datum: same head parameters, the remaining-accumulator commitment.
pfProgressOut :: Integer -> HS.FanoutProgressDatum
pfProgressOut tfinal =
  HS.FanoutProgressDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationDeadline = POSIXTime tfinal
    , HS.accumulatorCommitment = pfNewAccCommitment
    , HS.headAdaOverhead = fanoutOverhead
    }

-- head input = continuing head output + distributed output (value conservation, no token burn).
pfHeadOutVal :: Value
pfHeadOutVal = singleton headPolicy stName 1 <> singleton headPolicy ptName 1 <> singleton adaSymbol adaToken 2_000_000

pfHeadInVal :: Value
pfHeadInVal = pfHeadOutVal <> singleton adaSymbol adaToken 1_500_000

pfRedeemer :: Integer -> HS.Input
pfRedeemer m = HS.PartialFanout{HS.numberOfPartialOutputs = m, HS.crsRef = crsRefOut}

mkPartialContext :: Integer -> Integer -> Integer -> ScriptContext
mkPartialContext m validityLo tfinal =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headIn]
          , txInfoReferenceInputs = [pfCrsRefInput]
          , txInfoOutputs = [continuingOut, pfDistributedOut]
          , txInfoFee = 0
          , txInfoMint = emptyMintValue
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLo)) True) (UpperBound PosInf True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (pfRedeemer m))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Closed (pfClosedDatum tfinal)))))
    }
 where
  headIn = TxOut headAddr pfHeadInVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (pfClosedDatum tfinal))))) Nothing
  continuingOut = TxOut headAddr pfHeadOutVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.FanoutProgress (pfProgressOut tfinal))))) Nothing

-- the reference checks 0 < m AND tfinal < validityLo (args in that order).
partialRef :: Integer -> Integer -> Integer -> Bool
partialRef = Ref.checkPartialFanout

partialVal :: Integer -> Integer -> Integer -> Bool
partialVal m validityLo tfinal =
  Head.headValidator crsScriptHash (HS.Closed (pfClosedDatum tfinal)) (pfRedeemer m) (mkPartialContext m validityLo tfinal)

-- ── the agreement property ───────────────────────────────────────────────────────────────────────────

spec :: Spec
spec = parallel $ do
  -- Non-vacuity anchors: the agreement is not "both always reject". The validator genuinely ACCEPTS a
  -- well-formed CloseInitial and genuinely REJECTS one with a changed version, and the reference matches
  -- both. (Healthy: version'=0, cp'=100, snap'=0, contesters=0, deadline = tMax + cp.)
  prop "anchor: healthy CloseInitial — BOTH oracles accept" $
    let tMax = 1_100; deadline = tMax + openCpMs
     in validatorVerdict (mkContext 0 100 0 0 deadline tMax) === True
          .&&. referenceVerdict 0 100 0 0 deadline tMax === True

  prop "anchor: changed version — BOTH oracles reject" $
    let tMax = 1_100; deadline = tMax + openCpMs
     in validatorVerdict (mkContext 1 100 0 0 deadline tMax) === False
          .&&. referenceVerdict 1 100 0 0 deadline tMax === False

  prop "close/CloseInitial: extracted Agda reference === real validator (function-level, no tx, no mutation)" $
    forAll (choose (0, 2)) $ \closedVersion ->
      forAll (elements [50, 100, 200]) $ \closedCpMs ->
        forAll (choose (0, 1)) $ \closedSnap ->
          forAll (choose (0, 1)) $ \contestersLen ->
            forAll (elements [1_050, 1_100, 2_000]) $ \tMax ->
              forAll (elements [0, 1]) $ \deadlineExtra ->
                let deadline = tMax + openCpMs + deadlineExtra
                    ctx = mkContext closedVersion closedCpMs closedSnap contestersLen deadline tMax
                 in referenceVerdict closedVersion closedCpMs closedSnap contestersLen deadline tMax
                      === validatorVerdict ctx

  -- ── CloseUnused: the validator runs verifySnapshotSignature FOR REAL (signed with our test key) ──
  -- Anchor: a healthy CloseUnused (version preserved = 0, valid signature over the snapshot) is accepted by
  -- BOTH the real validator (signature verifies) and the reference.
  prop "anchor: healthy CloseUnused — BOTH oracles accept (real signature verified)" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in unusedVal (unusedRedeemer cs) 0 100 cs 0 dl tMax === True
          .&&. unusedRef 0 100 cs 0 dl tMax === True

  -- Agreement on the decidable conjuncts WITH a valid signature: reference === validator across the
  -- generated fields (the signature is valid, so crypto is not the deciding factor).
  prop "close/CloseUnused: reference === real validator (valid sig; decidable conjuncts)" $
    forAll (choose (0, 1)) $ \cv ->
      forAll (elements [50, 100]) $ \ccp ->
        forAll (choose (1, 3)) $ \cs ->
          forAll (choose (0, 1)) $ \cl ->
            forAll (elements [1_050, 2_000]) $ \tMax ->
              forAll (elements [0, 1]) $ \dExtra ->
                let dl = tMax + openCpMs + dExtra
                 in unusedRef cv ccp cs cl dl tMax === unusedVal (unusedRedeemer cs) cv ccp cs cl dl tMax

  -- Crypto non-vacuity (what the Agda CANNOT prove): the REAL validator rejects a CloseUnused whose
  -- signature is over the WRONG snapshot number, even though everything else is healthy. This genuinely
  -- exercises verifySnapshotSignature against the real validator (the reference mocks it, so this is a
  -- validator-only assertion).
  prop "close/CloseUnused: real validator REJECTS a bad-snapshot signature" $
    let cs = 3
        tMax = 1_100
        dl = tMax + openCpMs
        badRedeemer = HS.CloseUnused{HS.signature = [closeSigFor (cs + 1)], HS.accumulatorHash = emptyAccHash}
     in unusedVal badRedeemer 0 100 cs 0 dl tMax === False

  prop "close/CloseUnused: the healthy (correctly-signed) version of that tx IS accepted" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in unusedVal (unusedRedeemer cs) 0 100 cs 0 dl tMax === True

  -- ── increment: version bump + value conservation + a deposit script input + real signature ──
  prop "anchor: healthy increment — BOTH oracles accept (real signature verified)" $
    let cs = 3
     in incVal (incRedeemer cs) 1 0 === True .&&. incRef 1 0 === True

  prop "increment: reference === real validator (valid sig; version bump + value conservation)" $
    forAll (choose (0, 2)) $ \nextV ->
      forAll (elements [0, 1_000]) $ \vPerturb ->
        let cs = 3
         in incRef nextV vPerturb === incVal (incRedeemer cs) nextV vPerturb

  prop "increment: real validator REJECTS a bad snapshot signature" $
    let cs = 3
        badRedeemer = HS.IncrementRedeemer{HS.signature = [incSigFor (cs + 1)], HS.snapshotNumber = cs, HS.increment = depRef}
     in incVal badRedeemer 1 0 === False

  prop "increment: the healthy (correctly-signed) version of that tx IS accepted" $
    let cs = 3 in incVal (incRedeemer cs) 1 0 === True

  -- ── decrement: version bump + value shrinks by decommit outputs + real signature ──
  prop "anchor: healthy decrement — BOTH oracles accept (real signature verified)" $
    let cs = 3 in decVal (decRedeemer cs) 1 0 === True .&&. decRef 1 0 === True

  prop "decrement: reference === real validator (valid sig; version bump + value decrease)" $
    forAll (choose (0, 2)) $ \nextV ->
      forAll (elements [0, 1_000]) $ \vPerturb ->
        let cs = 3
         in decRef nextV vPerturb === decVal (decRedeemer cs) nextV vPerturb

  prop "decrement: real validator REJECTS a bad snapshot signature" $
    let cs = 3
        badRedeemer = HS.DecrementRedeemer{HS.signature = [incSigFor (cs + 1)], HS.snapshotNumber = cs, HS.numberOfDecommitOutputs = 1}
     in decVal badRedeemer 1 0 === False

  prop "decrement: the healthy (correctly-signed) version of that tx IS accepted" $
    let cs = 3 in decVal (decRedeemer cs) 1 0 === True

  -- ── contest: version preserved + snapshot increases + one contester + deadline + real signature ──
  prop "anchor: healthy contest — BOTH oracles accept (real signature verified)" $
    let s' = 1; tMax = 1_500
     in contestVal (contestRedeemer s') s' 0 tMax === True .&&. contestRef s' 0 tMax === True

  prop "contest: reference === real validator (valid sig; snapshot increase + deadline + within-period)" $
    forAll (choose (0, 2)) $ \s' ->
      forAll (elements [0, 100]) $ \tfinPerturb ->
        forAll (elements [1_500, 2_500]) $ \tMax ->
          contestRef s' tfinPerturb tMax === contestVal (contestRedeemer s') s' tfinPerturb tMax

  prop "contest: real validator REJECTS a bad snapshot signature" $
    let s' = 1
        tMax = 1_500
        badRedeemer = HS.ContestUnused{HS.signature = [contestSigFor (s' + 1)], HS.accumulatorHash = emptyAccHash}
     in contestVal badRedeemer s' 0 tMax === False

  prop "contest: the healthy (correctly-signed) version of that tx IS accepted" $
    let s' = 1; tMax = 1_500 in contestVal (contestRedeemer s') s' 0 tMax === True

  -- ── init (μHead): minted-token count + ST/PT placement in the head output ──
  prop "anchor: healthy init — BOTH oracles accept (mint 2, ST 1, 1 PT)" $
    initVal 2 1 1 === True .&&. initRef 2 1 1 === True

  prop "init: reference === real validator (minted count + ST quantity + unique-PT count)" $
    forAll (choose (1, 3)) $ \mintedCount ->
      forAll (choose (0, 2)) $ \stQty ->
        forAll (choose (0, 2)) $ \numPT ->
          initRef mintedCount stQty numPT === initVal mintedCount stQty numPT

  -- Non-vacuity for the conjuncts the reference MOCKS (OpsInit = const True): the REAL validator still
  -- enforces them. seedInputIsConsumed fails when the consumed input is not the seed.
  prop "init: real validator REJECTS when the seed input is not consumed" $
    let wrongRef = TxOutRef (TxId "88888888888888888888888888888888888888888888888888888888888888888888") 0
     in Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext wrongRef 2 1 1) === False

  prop "init: the healthy (seed-consumed) version of that tx IS accepted" $
    Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext initSeedRef 2 1 1) === True

  -- ── νDeposit Recover: the real Aiken validator (compiled UPLC) vs Ref.checkRecover ──
  prop "anchor: healthy recover — BOTH oracles accept (posted after the deadline)" $
    recoverVal 1_000 1_050 === True .&&. recoverRef 1_000 1_050 === True

  prop "recover: reference === real Aiken validator (after-deadline conjunct)" $
    forAll (elements [950, 1_000, 1_050]) $ \validityLo ->
      let deadline = 1_000
       in recoverRef deadline validityLo === recoverVal deadline validityLo

  -- ── νDeposit Claim: the real Aiken validator (compiled UPLC) vs Ref.checkClaim ──
  prop "anchor: healthy claim — BOTH oracles accept (before deadline + own-head increment)" $
    claimVal 1_000 950 headPolicy === True .&&. claimRef 1_000 950 headPolicy === True

  prop "claim: reference === real Aiken validator (before-deadline + own-head binding)" $
    forAll (elements [950, 1_000, 1_050]) $ \validityHi ->
      forAll (elements [headPolicy, otherHeadCid]) $ \depHeadCid ->
        let deadline = 1_000
         in claimRef deadline validityHi depHeadCid === claimVal deadline validityHi depHeadCid

  -- ── full fanout (empty head): real BLS membership (empty subset) + burn count + deadline + value ──
  prop "anchor: healthy empty-head fanout — BOTH oracles accept (real BLS pairing verified)" $
    fanoutVal 2 1_050 1_000 === True .&&. fanoutRef 2 1_050 1_000 === True

  prop "fanout: reference === real validator (burned-token count + after-deadline)" $
    forAll (choose (1, 3)) $ \burnedCount ->
      forAll (elements [950, 1_000, 1_050]) $ \validityLo ->
        let tfinal = 1_000
         in fanoutRef burnedCount validityLo tfinal === fanoutVal burnedCount validityLo tfinal

  -- ── partial fanout (real 2-element accumulator, distribute 1): membership + 0<m + after-deadline ──
  prop "anchor: healthy partial fanout — BOTH oracles accept (real KZG membership verified)" $
    partialVal 1 1_050 1_000 === True .&&. partialRef 1 1_000 1_050 === True

  prop "partial fanout: reference === real validator (mustHaveOutputs 0<m + after-deadline)" $
    forAll (elements [0, 1]) $ \m ->
      forAll (elements [950, 1_050]) $ \validityLo ->
        let tfinal = 1_000
         in partialRef m tfinal validityLo === partialVal m validityLo tfinal
