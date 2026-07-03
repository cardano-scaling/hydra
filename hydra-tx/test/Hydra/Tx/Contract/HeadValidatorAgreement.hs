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

-- ── close value preservation demo (C3.4): mustPreserveHeadValue is the EXACT `==` on the head value ──
-- A CloseInitial healthy in every other conjunct, with the head OUTPUT value's ada parameterized: equal to
-- the input (2_000_000) is accepted; siphoned (< input) is rejected by mustPreserveHeadValue. Exercises the
-- extracted checkValuePreserved (now bridged from closeValid.valuePreserved) against the real validator.
mkCloseValueContext :: Integer -> ScriptContext
mkCloseValueContext headOutAda =
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
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Close HS.CloseInitial))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open openDatum))))
    }
 where
  tMax = 1_100
  deadline = tMax + openCpMs
  headInputOut = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open openDatum)))) Nothing
  headOutputOut =
    TxOut
      headAddr
      (singleton adaSymbol adaToken headOutAda <> singleton headPolicy ptName 1)
      (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (closedDatum 0 100 0 0 deadline)))))
      Nothing

closeValueVal :: Integer -> Bool
closeValueVal headOutAda = validatorVerdict (mkCloseValueContext headOutAda)

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

-- ── signed CloseAny: like CloseUnused (signature over the CURRENT version) PLUS snapshot number > 0 ─────
-- The validator's CloseAny arm additionally requires snapshotNumber' > 0; the reference models the same
-- via the anyOK conjunct on the CloseAnyT tag. The signature message is identical to CloseUnused's (open
-- version 0), so the scaffolding (openDatumU, mkContextU, closeSigFor) is shared; only the redeemer and
-- the reference tag differ.
anyRedeemer :: Integer -> HS.CloseRedeemer
anyRedeemer cs = HS.CloseAny{HS.signature = [closeSigFor cs], HS.accumulatorHash = emptyAccHash}

anyRef :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
anyRef cv ccp cs cl dl tMax =
  Ref.checkClose
    (Ref.mkOps (\_ _ _ -> True))
    (Ref.MkOpen openVersionN openCpMs)
    (Ref.MkClosed cv ccp cs cl dl)
    Ref.CloseAnyT
    tMax
    validityLoN

anyVal :: HS.CloseRedeemer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
anyVal = unusedVal

-- ── signed CloseUsed: the signature is over version - 1 (a pending inc/dec was already applied) ─────────
-- The validator's CloseUsed arm verifies the snapshot signature at the PREVIOUS open version. We open at
-- version 1, so version - 1 = 0 and the message is exactly `closeMsg` again (Plutus Integer subtraction
-- and the Agda v ∸ 1 agree away from 0; the v = 0 corner lives inside the mocked crypto boundary). The
-- reference sees the same fields under the CloseUsedT tag; version preservation now demands cv = 1.
usedOpenVersionN :: Integer
usedOpenVersionN = 1

-- openDatumU at version 1; headSeed (unique to OpenDatum) pins the record-update type.
openDatumUsed :: HS.OpenDatum
openDatumUsed = openDatumU{HS.headSeed = ownRef, HS.version = usedOpenVersionN}

usedRedeemer :: Integer -> HS.CloseRedeemer
usedRedeemer cs = HS.CloseUsed{HS.signature = [closeSigFor cs], HS.accumulatorHash = emptyAccHash}

-- mkContextU with the version-1 open datum (each family keeps its own builder).
mkContextUsed :: HS.CloseRedeemer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ScriptContext
mkContextUsed redeemer cv ccp cs cl dl tMax =
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
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open openDatumUsed))))
    }
 where
  headInU = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open openDatumUsed)))) Nothing
  headOutU = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (closedDatumU cv ccp cs cl dl))))) Nothing

usedRef :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
usedRef cv ccp cs cl dl tMax =
  Ref.checkClose
    (Ref.mkOps (\_ _ _ -> True))
    (Ref.MkOpen usedOpenVersionN openCpMs)
    (Ref.MkClosed cv ccp cs cl dl)
    Ref.CloseUsedT
    tMax
    validityLoN

usedVal :: HS.CloseRedeemer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
usedVal redeemer cv ccp cs cl dl tMax =
  Head.headValidator headScriptHash (HS.Open openDatumUsed) (HS.Close redeemer) (mkContextUsed redeemer cv ccp cs cl dl tMax)

-- CloseUsed hash-vs-datum coupling (mustBindAccumulatorCommitment): the redeemer's accumulatorHash must
-- be the blake2b of the PRODUCED datum's commitment. Here the redeemer carries the hash of a DIFFERENT
-- G1 point (2·G) and the signature is over that same wrong hash, so verifySnapshotSignature ACCEPTS and
-- only the datum binding fails (a validator-only conjunct; the reference mocks the accumulator).
usedWrongAccHash :: Builtins.BuiltinByteString
usedWrongAccHash = Builtins.blake2b_256 (Builtins.bls12_381_G1_compress (Builtins.bls12_381_G1_add g1Generator g1Generator))

usedRedeemerWrongHash :: Integer -> HS.CloseRedeemer
usedRedeemerWrongHash cs =
  HS.CloseUsed{HS.signature = [Builtins.toBuiltin (rawSerialiseSigDSIGN (signDSIGN () msg snapshotSK))], HS.accumulatorHash = usedWrongAccHash}
 where
  -- the CloseUsed message at open version 1 signs the version slot v - 1 = 0 (= openVersionN).
  msg :: ByteString
  msg =
    Builtins.fromBuiltin $
      Builtins.serialiseData (PlutusTx.toBuiltinData headPolicy)
        <> Builtins.serialiseData (PlutusTx.toBuiltinData openVersionN)
        <> Builtins.serialiseData (PlutusTx.toBuiltinData cs)
        <> Builtins.serialiseData (PlutusTx.toBuiltinData usedWrongAccHash)

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

-- ── increment conjunct demos: extracted conjunct checkers the family `checkInc` agreement does NOT exercise ──
-- The family test above holds no-mint / participant / per-asset HEALTHY and varies only version + value
-- totals. These targeted demos construct a single-conjunct attack and assert BOTH the extracted conjunct
-- checker AND the real validator reject it (and accept the healthy form), keeping the coverage the deleted
-- mutation-based `Differential.hs` had for these conjuncts. The healthy increment is `incRedeemer 3`,
-- nextV = 1, deposit 500_000; the snapshot signature is valid throughout, so each attack isolates one check.

-- a flexible increment context for the demos: knobs are the mint, the tx signatories, and the head input /
-- output values; everything else is the healthy increment (valid sig, deposit, version bump).
mkIncDemoContext :: MintValue -> [PubKeyHash] -> Value -> Value -> ScriptContext
mkIncDemoContext mint signers hInVal hOutVal =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [TxInInfo ownRef headIn, TxInInfo depRef depIn]
          , txInfoReferenceInputs = []
          , txInfoOutputs = [headOut]
          , txInfoFee = 0
          , txInfoMint = mint
          , txInfoTxCerts = []
          , txInfoWdrl = AMap.empty
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime 2_000)) True)
          , txInfoSignatories = signers
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Increment (incRedeemer 3)))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev))))
    }
 where
  headIn = TxOut headAddr hInVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open incOpenPrev)))) Nothing
  depIn = TxOut depAddr depVal (OutputDatum (Datum (PlutusTx.toBuiltinData (0 :: Integer)))) Nothing
  headOut = TxOut headAddr hOutVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open (incOpenNext 1))))) Nothing

incDemoVal :: ScriptContext -> Bool
incDemoVal = Head.headValidator headScriptHash (HS.Open incOpenPrev) (HS.Increment (incRedeemer 3))

-- big-endian integer encoding of a builtin byte string (equal iff the bytes are equal): the encoding the
-- extracted participant/cid checkers compare on the Haskell side.
bytesToInteger :: Builtins.BuiltinByteString -> Integer
bytesToInteger = BS.foldl' (\acc w -> acc * 256 + toInteger w) 0 . Builtins.fromBuiltin

-- no-mint attack: an increment that mints a head-policy token (any non-zero mint breaks mustNotMintOrBurn).
incAttackMint :: MintValue
incAttackMint = UnsafeMintValue (AMap.unsafeFromList [(headPolicy, AMap.unsafeFromList [(TokenName (Builtins.toBuiltin ("\11" :: ByteString)), 1)])])

incHealthyHeadOut :: Value
incHealthyHeadOut = incHeadVal <> depVal

-- participant attack: a signer whose key-hash is NOT a participation-token name in the head value.
nonParticipantKH :: PubKeyHash
nonParticipantKH = PubKeyHash "99999999999999999999999999999999999999999999999999999999"

-- the extracted participant checker: tx signers' key-hashes vs the head value's PT names must OVERLAP.
participantRef :: [PubKeyHash] -> Bool
participantRef signers =
  Ref.checkParticipantSigned (Ref.MkSignerIO (map (bytesToInteger . getPubKeyHash) signers) [bytesToInteger (unTokenName ptName)])

-- per-asset attack: a balanced A→B token swap (the non-ada TOTAL is preserved, but one asset is not).
tokenA :: TokenName
tokenA = TokenName (Builtins.toBuiltin ("token-A" :: ByteString))

tokenB :: TokenName
tokenB = TokenName (Builtins.toBuiltin ("token-B" :: ByteString))

incPerAssetHeadIn :: Value
incPerAssetHeadIn = incHeadVal <> singleton headPolicy tokenA 1

incPerAssetHeadOutHealthy :: Value
incPerAssetHeadOutHealthy = incHeadVal <> depVal <> singleton headPolicy tokenA 1

incPerAssetHeadOutSwap :: Value
incPerAssetHeadOutSwap = incHeadVal <> depVal <> singleton headPolicy tokenB 1

-- ref-spent attack (the increment claimedDepositIsSpent): the redeemer claims a deposit out-ref that is
-- NOT among the tx inputs. Only that conjunct breaks (the signature message does not cover the claimed
-- ref, and the deposit input still feeds mustPreserveValue), so both the extracted checkRefSpent and the
-- real validator must reject; the healthy claim (= depRef) passes both.
unspentDepRef :: TxOutRef
unspentDepRef = TxOutRef (TxId "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee") 0

-- deterministic injective encoding of an out-ref (txid bytes big-endian, then the index): equal iff the
-- out-refs are equal for index < 256 (all fixtures use index 0). The one encoding both checkRefSpent
-- arguments share.
encodeTxOutRef :: TxOutRef -> Integer
encodeTxOutRef (TxOutRef (TxId tid) ix) = bytesToInteger tid * 256 + ix

-- the increment tx's spent input out-refs (head + deposit), under the same encoding.
incInputRefCodes :: [Integer]
incInputRefCodes = encodeTxOutRef <$> [ownRef, depRef]

incRedeemerUnspent :: Integer -> HS.IncrementRedeemer
incRedeemerUnspent snap = HS.IncrementRedeemer{HS.signature = [incSigFor snap], HS.snapshotNumber = snap, HS.increment = unspentDepRef}

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

-- ── contest mustNotChangeParameters demo (C3.3): headId preservation (bridged from the contest transition) ──
-- A healthy ContestUnused (s'=1) whose PRODUCED datum changes the head id. The snapshot signature is over the
-- INPUT head id, so it still verifies; only mustNotChangeParameters fails. Built explicitly (headId is a
-- duplicate field, so a record update would be ambiguous).
contestNextBadHeadId :: HS.ClosedDatum
contestNextBadHeadId =
  HS.ClosedDatum
    { HS.headId = otherHeadCid
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = 0
    , HS.snapshotNumber = 1
    , HS.contesters = [signerKH]
    , HS.contestationDeadline = POSIXTime 2_000
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = 0
    }

-- a contest context with an explicitly-supplied produced datum (otherwise the healthy s'=1 contest).
mkContestParamsContext :: HS.ClosedDatum -> ScriptContext
mkContestParamsContext producedDatum =
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
          , txInfoValidRange = Interval (LowerBound (Finite (POSIXTime validityLoN)) True) (UpperBound (Finite (POSIXTime 1_500)) True)
          , txInfoSignatories = [signerKH]
          , txInfoRedeemers = AMap.empty
          , txInfoData = AMap.empty
          , txInfoId = TxId "44444444444444444444444444444444444444444444444444444444444444444444"
          , txInfoVotes = AMap.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
    , scriptContextRedeemer = Redeemer (PlutusTx.toBuiltinData (HS.Contest (contestRedeemer 1)))
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Closed contestPrev))))
    }
 where
  headIn = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed contestPrev)))) Nothing
  headOut = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed producedDatum)))) Nothing

contestParamsVal :: HS.ClosedDatum -> Bool
contestParamsVal producedDatum =
  Head.headValidator headScriptHash (HS.Closed contestPrev) (HS.Contest (contestRedeemer 1)) (mkContestParamsContext producedDatum)

-- ── signed ContestUsed: the contest signature is over version - 1 (pending inc/dec already applied) ─────
-- The validator's ContestUsed arm verifies the snapshot signature at the PREVIOUS version. The closed
-- input carries version 1, so version - 1 = 0 and the message is exactly `contestMsg` again (as with
-- CloseUsed, the v = 0 monus corner lives inside the mocked crypto boundary). The reference has no
-- contest tag: the redeemer choice only moves the signature message, which is the injected conjunct.
contestUsedPrev :: HS.ClosedDatum
contestUsedPrev =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = 1
    , HS.snapshotNumber = 0
    , HS.contesters = []
    , HS.contestationDeadline = POSIXTime 2_000
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = 0
    }

contestUsedNext :: Integer -> Integer -> HS.ClosedDatum
contestUsedNext sPrime tfinPerturb =
  contestUsedPrev{HS.snapshotNumber = sPrime, HS.contesters = [signerKH], HS.contestationDeadline = POSIXTime (2_000 + tfinPerturb)}

contestUsedRedeemer :: Integer -> HS.ContestRedeemer
contestUsedRedeemer sPrime = HS.ContestUsed{HS.signature = [contestSigFor sPrime], HS.accumulatorHash = emptyAccHash}

mkContestUsedContext :: HS.ContestRedeemer -> Integer -> Integer -> Integer -> ScriptContext
mkContestUsedContext redeemer sPrime tfinPerturb tMax =
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
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Closed contestUsedPrev))))
    }
 where
  headIn = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed contestUsedPrev)))) Nothing
  headOut = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (contestUsedNext sPrime tfinPerturb))))) Nothing

contestUsedRef :: Integer -> Integer -> Integer -> Bool
contestUsedRef sPrime tfinPerturb tMax =
  Ref.checkContest
    (Ref.mkOpsContest (const True))
    (Ref.MkContestIO 1 1 0 sPrime 0 1 2_000 tMax (2_000 + tfinPerturb) 1 openCpMs)

contestUsedVal :: HS.ContestRedeemer -> Integer -> Integer -> Integer -> Bool
contestUsedVal redeemer sPrime tfinPerturb tMax =
  Head.headValidator headScriptHash (HS.Closed contestUsedPrev) (HS.Contest redeemer) (mkContestUsedContext redeemer sPrime tfinPerturb tMax)

-- ── contest deadline-push (n = 2): the contest does NOT complete the round, so tfinal' = tfinal + cp ───
-- With one party the deadline-push accept branch is unreachable (one contester is all of them), so the
-- n = 1 family above only exercises the tfinal' = tfinal side of mustPushDeadline. Here TWO parties hold
-- the head and ONE contests: contesters' (1) /= parties' (2), so the produced datum must record
-- tfinal + cp. The snapshot multisignature needs BOTH parties' keys (verifySnapshotSignature zips
-- parties with signatures), so a second deterministic Ed25519 key joins the head.
snapshotSK2 :: SignKeyDSIGN Ed25519DSIGN
snapshotSK2 = genKeyDSIGN (mkSeedFromBytes (digest (Proxy :: Proxy SHA256) ("hva-snapshot-seed-2" :: ByteString)))

snapshotParty2 :: Party
snapshotParty2 = partyFromVerificationKeyBytes (rawSerialiseVerKeyDSIGN (deriveVerKeyDSIGN snapshotSK2))

contest2Prev :: HS.ClosedDatum
contest2Prev =
  HS.ClosedDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty, snapshotParty2]
    , HS.contestationPeriod = UnsafeContestationPeriod (fromInteger openCpMs)
    , HS.version = 0
    , HS.snapshotNumber = 0
    , HS.contesters = []
    , HS.contestationDeadline = POSIXTime 2_000
    , HS.accumulatorCommitment = g1Generator
    , HS.headAdaOverhead = 0
    }

contest2Next :: Integer -> Integer -> HS.ClosedDatum
contest2Next sPrime tfinPerturb =
  contest2Prev{HS.snapshotNumber = sPrime, HS.contesters = [signerKH], HS.contestationDeadline = POSIXTime (2_000 + tfinPerturb)}

-- both parties sign the same ContestUnused message (version 0), in parties order.
contest2SigsFor :: Integer -> [HS.Signature]
contest2SigsFor sPrime =
  [ contestSigFor sPrime
  , Builtins.toBuiltin (rawSerialiseSigDSIGN (signDSIGN () (contestMsg sPrime) snapshotSK2))
  ]

contest2Redeemer :: Integer -> HS.ContestRedeemer
contest2Redeemer sPrime = HS.ContestUnused{HS.signature = contest2SigsFor sPrime, HS.accumulatorHash = emptyAccHash}

mkContest2Context :: HS.ContestRedeemer -> Integer -> Integer -> Integer -> ScriptContext
mkContest2Context redeemer sPrime tfinPerturb tMax =
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
    , scriptContextScriptInfo = SpendingScript ownRef (Just (Datum (PlutusTx.toBuiltinData (HS.Closed contest2Prev))))
    }
 where
  headIn = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed contest2Prev)))) Nothing
  headOut = TxOut headAddr headVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Closed (contest2Next sPrime tfinPerturb))))) Nothing

-- reference with numParties = 2: the deadline-update rule now demands tfinal' = tfinal + cp.
contest2Ref :: Integer -> Integer -> Integer -> Bool
contest2Ref sPrime tfinPerturb tMax =
  Ref.checkContest
    (Ref.mkOpsContest (const True))
    (Ref.MkContestIO 0 0 0 sPrime 0 1 2_000 tMax (2_000 + tfinPerturb) 2 openCpMs)

contest2Val :: HS.ContestRedeemer -> Integer -> Integer -> Integer -> Bool
contest2Val redeemer sPrime tfinPerturb tMax =
  Head.headValidator headScriptHash (HS.Closed contest2Prev) (HS.Contest redeemer) (mkContest2Context redeemer sPrime tfinPerturb tMax)

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
mkInitContext :: TxOutRef -> HS.OpenDatum -> Integer -> Integer -> Integer -> ScriptContext
mkInitContext inputSeedRef headDatum mintedCount stQty numPT =
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
  headOut = TxOut headAddr (initHeadVal stQty numPT) (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.Open headDatum)))) Nothing

initRef :: Integer -> Integer -> Integer -> Bool
initRef mintedCount stQty numPT =
  Ref.checkInit (Ref.mkOpsInit (const True)) (Ref.MkMintIO 1 mintedCount stQty (stQty + numPT))

initVal :: Integer -> Integer -> Integer -> Bool
initVal mintedCount stQty numPT =
  Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext initSeedRef initOpenDatum mintedCount stQty numPT)

-- ── init datum head-id binding demo (C3.2): μHead checkDatum requires datum.headId == currency ──
-- The head output datum names a different head id than the minting policy → checkDatum (WrongDatum) rejects.
-- headSeed (unique to OpenDatum) pins the record-update type so the headId update is unambiguous.
initOpenDatumBadHeadId :: HS.OpenDatum
initOpenDatumBadHeadId = initOpenDatum{HS.headId = otherHeadCid, HS.headSeed = initSeedRef}

initHeadIdVal :: HS.OpenDatum -> Bool
initHeadIdVal od = Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext initSeedRef od 2 1 1)

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
cidToInteger = bytesToInteger . unCurrencySymbol

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

-- A WRONG BLS membership proof: a G1 point ≠ the empty-accumulator commitment (g1Generator). For the
-- empty head (m = 0) checkMembershipPairing reduces to `commitment == proof`, so feeding a mismatched
-- proof makes the REAL bls12_381 pairing check FAIL — exercising the BLS crypto in the reject direction
-- (the reference mocks this conjunct, so only the real validator sees it). 2·G ≠ G.
fanoutBadProof :: Builtins.BuiltinBLS12_381_G1_Element
fanoutBadProof = Builtins.bls12_381_G1_add g1Generator g1Generator

fanoutValBadProof :: Integer -> Integer -> Integer -> Bool
fanoutValBadProof burnedCount validityLo tfinal =
  Head.headValidator
    crsScriptHash
    (HS.Closed (fanoutClosedDatum tfinal))
    (HS.Fanout{HS.numberOfFanoutOutputs = 0, HS.proof = fanoutBadProof, HS.crsRef = crsRefOut})
    (mkFanoutContext burnedCount validityLo tfinal)

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

-- NB: argument order DIFFERS from partialRef above — here it is (m, validityLo, tfinal),
-- there it is (m, tfinal, validityLo). Every call site below swaps the last two to match.
partialVal :: Integer -> Integer -> Integer -> Bool
partialVal m validityLo tfinal =
  Head.headValidator crsScriptHash (HS.Closed (pfClosedDatum tfinal)) (pfRedeemer m) (mkPartialContext m validityLo tfinal)

-- A WRONG KZG membership: the continuing FanoutProgress output claims the head did NOT shrink (its
-- commitment = the OLD full-accumulator commitment), so e(oldAcc, G2) == e(newAcc, P_S(τ)·G2) fails
-- against the real CRS — exercising the KZG pairing in the reject direction. Value is unchanged, so
-- the ONLY failing check is the membership pairing. (Built explicitly: accumulatorCommitment is a
-- duplicate field, so a record update would be ambiguous.)
pfProgressOutBad :: Integer -> HS.FanoutProgressDatum
pfProgressOutBad tfinal =
  HS.FanoutProgressDatum
    { HS.headId = headPolicy
    , HS.parties = [snapshotParty]
    , HS.contestationDeadline = POSIXTime tfinal
    , HS.accumulatorCommitment = pfInputAccCommitment
    , HS.headAdaOverhead = fanoutOverhead
    }

mkPartialContextBadProof :: Integer -> Integer -> Integer -> ScriptContext
mkPartialContextBadProof m validityLo tfinal =
  base{scriptContextTxInfo = (scriptContextTxInfo base){txInfoOutputs = [continuingOutBad, pfDistributedOut]}}
 where
  base = mkPartialContext m validityLo tfinal
  continuingOutBad = TxOut headAddr pfHeadOutVal (OutputDatum (Datum (PlutusTx.toBuiltinData (HS.FanoutProgress (pfProgressOutBad tfinal))))) Nothing

partialValBadProof :: Integer -> Integer -> Integer -> Bool
partialValBadProof m validityLo tfinal =
  Head.headValidator crsScriptHash (HS.Closed (pfClosedDatum tfinal)) (pfRedeemer m) (mkPartialContextBadProof m validityLo tfinal)

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

  -- ── close mustPreserveHeadValue (C3.4: bridged from closeValid.valuePreserved + tested here) ──
  prop "close/value: a siphoned head output is REJECTED by both checkValuePreserved and the real validator" $
    Ref.checkValuePreserved 2_000_000 1_500_000 1 1 === False
      .&&. closeValueVal 1_500_000 === False
  prop "close/value: the value-preserving close is accepted by both" $
    Ref.checkValuePreserved 2_000_000 2_000_000 1 1 === True
      .&&. closeValueVal 2_000_000 === True

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

  -- ── CloseAny: signature over the CURRENT version PLUS snapshot number > 0 (the anyOK conjunct) ──
  prop "anchor: healthy CloseAny, BOTH oracles accept (real signature verified, snapshot > 0)" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in anyVal (anyRedeemer cs) 0 100 cs 0 dl tMax === True
          .&&. anyRef 0 100 cs 0 dl tMax === True

  -- Agreement across a grid that INCLUDES snapshot number 0: there the signature still verifies (it is
  -- over the same snapshot-0 message) but the validator's `snapshotNumber' > 0` and the reference's
  -- anyOK both reject, so the tag-specific conjunct is exercised in both directions.
  prop "close/CloseAny: reference === real validator (valid sig; includes snapshot 0)" $
    forAll (choose (0, 1)) $ \cv ->
      forAll (elements [50, 100]) $ \ccp ->
        forAll (choose (0, 3)) $ \cs ->
          forAll (choose (0, 1)) $ \cl ->
            forAll (elements [1_050, 2_000]) $ \tMax ->
              forAll (elements [0, 1]) $ \dExtra ->
                let dl = tMax + openCpMs + dExtra
                 in anyRef cv ccp cs cl dl tMax === anyVal (anyRedeemer cs) cv ccp cs cl dl tMax

  prop "close/CloseAny: snapshot number 0, BOTH oracles reject" $
    let tMax = 1_100; dl = tMax + openCpMs
     in anyVal (anyRedeemer 0) 0 100 0 0 dl tMax === False
          .&&. anyRef 0 100 0 0 dl tMax === False

  prop "close/CloseAny: real validator REJECTS a bad-snapshot signature" $
    let cs = 3
        tMax = 1_100
        dl = tMax + openCpMs
        badRedeemer = HS.CloseAny{HS.signature = [closeSigFor (cs + 1)], HS.accumulatorHash = emptyAccHash}
     in anyVal badRedeemer 0 100 cs 0 dl tMax === False

  prop "close/CloseAny: the healthy (correctly-signed) version of that tx IS accepted" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in anyVal (anyRedeemer cs) 0 100 cs 0 dl tMax === True

  -- ── CloseUsed: signature over version - 1 (open version 1 here, so the message is at version 0) ──
  prop "anchor: healthy CloseUsed, BOTH oracles accept (real signature at version - 1 verified)" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in usedVal (usedRedeemer cs) usedOpenVersionN 100 cs 0 dl tMax === True
          .&&. usedRef usedOpenVersionN 100 cs 0 dl tMax === True

  -- Agreement on the decidable conjuncts WITH a valid signature: cv spans 0/1/2, so the
  -- version-preservation boundary (accept only at cv = 1) is crossed in both directions.
  prop "close/CloseUsed: reference === real validator (valid sig; decidable conjuncts)" $
    forAll (choose (0, 2)) $ \cv ->
      forAll (elements [50, 100]) $ \ccp ->
        forAll (choose (1, 3)) $ \cs ->
          forAll (choose (0, 1)) $ \cl ->
            forAll (elements [1_050, 2_000]) $ \tMax ->
              forAll (elements [0, 1]) $ \dExtra ->
                let dl = tMax + openCpMs + dExtra
                 in usedRef cv ccp cs cl dl tMax === usedVal (usedRedeemer cs) cv ccp cs cl dl tMax

  prop "close/CloseUsed: real validator REJECTS a bad-snapshot signature" $
    let cs = 3
        tMax = 1_100
        dl = tMax + openCpMs
        badRedeemer = HS.CloseUsed{HS.signature = [closeSigFor (cs + 1)], HS.accumulatorHash = emptyAccHash}
     in usedVal badRedeemer usedOpenVersionN 100 cs 0 dl tMax === False

  -- The hash-vs-datum coupling in isolation: the signature over the WRONG hash verifies, so the reject
  -- comes from mustBindAccumulatorCommitment alone (the reference mocks the accumulator conjunct).
  prop "close/CloseUsed: real validator REJECTS a redeemer hash that does not match the datum commitment" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in usedVal (usedRedeemerWrongHash cs) usedOpenVersionN 100 cs 0 dl tMax === False

  prop "close/CloseUsed: the healthy (correctly-signed) version of that tx IS accepted" $
    let cs = 3; tMax = 1_100; dl = tMax + openCpMs
     in usedVal (usedRedeemer cs) usedOpenVersionN 100 cs 0 dl tMax === True

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

  -- ── increment conjunct demos (extracted checker + real validator both catch a single-conjunct attack) ──
  prop "increment/no-mint: a minting increment is REJECTED by both checkNoMint and the real validator" $
    Ref.checkNoMint 1 === False
      .&&. incDemoVal (mkIncDemoContext incAttackMint [signerKH] incHeadVal incHealthyHeadOut) === False
  prop "increment/no-mint: the healthy (no-mint) increment is accepted by both" $
    Ref.checkNoMint 0 === True
      .&&. incDemoVal (mkIncDemoContext emptyMintValue [signerKH] incHeadVal incHealthyHeadOut) === True

  prop "increment/participant: a non-participant signer is REJECTED by both checkParticipantSigned and the real validator" $
    participantRef [nonParticipantKH] === False
      .&&. incDemoVal (mkIncDemoContext emptyMintValue [nonParticipantKH] incHeadVal incHealthyHeadOut) === False
  prop "increment/participant: a participant signer is accepted by both" $
    participantRef [signerKH] === True
      .&&. incDemoVal (mkIncDemoContext emptyMintValue [signerKH] incHeadVal incHealthyHeadOut) === True

  -- a balanced A→B swap keeps the non-ada TOTAL (3 in, 3 out), so the scalar-total checkInc accepts it,
  -- but per-asset conservation (and the validator's Value ==) does not.
  prop "increment/per-asset: a balanced token swap passes the non-ada TOTAL but is REJECTED by checkPerAsset and the real validator" $
    Ref.checkInc (Ref.mkOpsInc (const True)) (Ref.MkIncIO openVersionN 1 2_000_000 500_000 2_500_000 3 0 3) === True
      .&&. Ref.checkPerAsset [Ref.MkAssetIO 1 0 1, Ref.MkAssetIO 1 0 1, Ref.MkAssetIO 1 0 0, Ref.MkAssetIO 0 0 1] === False
      .&&. incDemoVal (mkIncDemoContext emptyMintValue [signerKH] incPerAssetHeadIn incPerAssetHeadOutSwap) === False
  prop "increment/per-asset: the healthy (no swap) increment is accepted by both checkPerAsset and the real validator" $
    Ref.checkPerAsset [Ref.MkAssetIO 1 0 1, Ref.MkAssetIO 1 0 1, Ref.MkAssetIO 1 0 1] === True
      .&&. incDemoVal (mkIncDemoContext emptyMintValue [signerKH] incPerAssetHeadIn incPerAssetHeadOutHealthy) === True

  prop "increment/ref-spent: a claimed deposit that is NOT a tx input is REJECTED by both checkRefSpent and the real validator" $
    Ref.checkRefSpent (encodeTxOutRef unspentDepRef) incInputRefCodes === False
      .&&. incVal (incRedeemerUnspent 3) 1 0 === False
  prop "increment/ref-spent: the healthy (spent-deposit) claim is accepted by both" $
    Ref.checkRefSpent (encodeTxOutRef depRef) incInputRefCodes === True
      .&&. incVal (incRedeemer 3) 1 0 === True

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

  -- ── contest mustNotChangeParameters (C3.3: bridged from the contest transition + tested here) ──
  prop "contest/params: a changed head id is REJECTED by both checkContestParams and the real validator" $
    Ref.checkContestParams (cidToInteger headPolicy) (cidToInteger otherHeadCid) openCpMs openCpMs === False
      .&&. contestParamsVal contestNextBadHeadId === False
  prop "contest/params: the parameter-preserving contest is accepted by both" $
    Ref.checkContestParams (cidToInteger headPolicy) (cidToInteger headPolicy) openCpMs openCpMs === True
      .&&. contestParamsVal (contestNext 1 0) === True

  -- ── ContestUsed: the contest signature is over version - 1 (closed version 1 here) ──
  prop "anchor: healthy ContestUsed, BOTH oracles accept (real signature at version - 1 verified)" $
    let s' = 1; tMax = 1_500
     in contestUsedVal (contestUsedRedeemer s') s' 0 tMax === True .&&. contestUsedRef s' 0 tMax === True

  prop "contest/ContestUsed: reference === real validator (valid sig; snapshot increase + deadline + within-period)" $
    forAll (choose (0, 2)) $ \s' ->
      forAll (elements [0, 100]) $ \tfinPerturb ->
        forAll (elements [1_500, 2_500]) $ \tMax ->
          contestUsedRef s' tfinPerturb tMax === contestUsedVal (contestUsedRedeemer s') s' tfinPerturb tMax

  prop "contest/ContestUsed: real validator REJECTS a bad snapshot signature" $
    let s' = 1
        tMax = 1_500
        badRedeemer = HS.ContestUsed{HS.signature = [contestSigFor (s' + 1)], HS.accumulatorHash = emptyAccHash}
     in contestUsedVal badRedeemer s' 0 tMax === False

  prop "contest/ContestUsed: the healthy (correctly-signed) version of that tx IS accepted" $
    let s' = 1; tMax = 1_500 in contestUsedVal (contestUsedRedeemer s') s' 0 tMax === True

  -- ── contest deadline-push (n = 2, 1 contester): the produced deadline MUST be tfinal + cp ──
  prop "anchor: n = 2 contest with a pushed deadline, BOTH oracles accept (2-of-2 signature verified)" $
    let s' = 1; tMax = 1_500
     in contest2Val (contest2Redeemer s') s' openCpMs tMax === True
          .&&. contest2Ref s' openCpMs tMax === True

  prop "contest/deadline-push (n=2): a NON-pushed deadline is rejected by both (mustPushDeadline)" $
    let s' = 1; tMax = 1_500
     in contest2Val (contest2Redeemer s') s' 0 tMax === False
          .&&. contest2Ref s' 0 tMax === False

  prop "contest/deadline-push (n=2): reference === real validator (deadline-update rule both directions)" $
    forAll (choose (0, 2)) $ \s' ->
      forAll (elements [0, openCpMs, 2 * openCpMs]) $ \tfinPerturb ->
        forAll (elements [1_500, 2_500]) $ \tMax ->
          contest2Ref s' tfinPerturb tMax === contest2Val (contest2Redeemer s') s' tfinPerturb tMax

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
     in Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext wrongRef initOpenDatum 2 1 1) === False

  prop "init: the healthy (seed-consumed) version of that tx IS accepted" $
    Tokens.validateTokensMinting headScriptHash initSeedRef (mkInitContext initSeedRef initOpenDatum 2 1 1) === True

  -- ── init datum head-id binding (C3.2: bridged from the spec's cid identity + tested here) ──
  prop "init/datum: a head output datum naming a different head id is REJECTED by both checkInitHeadId and the real policy" $
    Ref.checkInitHeadId (cidToInteger otherHeadCid) (cidToInteger headPolicy) === False
      .&&. initHeadIdVal initOpenDatumBadHeadId === False
  prop "init/datum: the head-id-binding datum is accepted by both" $
    Ref.checkInitHeadId (cidToInteger headPolicy) (cidToInteger headPolicy) === True
      .&&. initHeadIdVal initOpenDatum === True

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

  prop "fanout: real validator REJECTS a wrong BLS membership proof (healthy accepts, bad proof rejects)" $
    fanoutVal 2 1_050 1_000 === True .&&. fanoutValBadProof 2 1_050 1_000 === False

  -- ── partial fanout (real 2-element accumulator, distribute 1): membership + 0<m + after-deadline ──
  prop "anchor: healthy partial fanout — BOTH oracles accept (real KZG membership verified)" $
    partialVal 1 1_050 1_000 === True .&&. partialRef 1 1_000 1_050 === True

  prop "partial fanout: reference === real validator (mustHaveOutputs 0<m + after-deadline)" $
    forAll (elements [0, 1]) $ \m ->
      forAll (elements [950, 1_050]) $ \validityLo ->
        let tfinal = 1_000
         in partialRef m tfinal validityLo === partialVal m validityLo tfinal

  prop "partial fanout: real validator REJECTS a wrong KZG membership proof (healthy accepts, bad proof rejects)" $
    partialVal 1 1_050 1_000 === True .&&. partialValBadProof 1 1_050 1_000 === False

  -- ── C3.5: the JOIN as one checked artifact ──
  -- The bridge proves spec-bundle ⇒ extracted-reference (Agda). This single property checks the other half,
  -- extracted-reference === real-validator, on the SAME inputs across EVERY validator family (one accept +
  -- one reject each), so the end-to-end spec ⇒ validator chain (modulo the documented postulates) is a
  -- single named, checked artifact rather than per-family scattered tests.
  prop "end-to-end (join): the bridged reference === the real validator across every family (accept + reject)" $
    let dl = 1_200; tMax = 1_100
     in -- close (CloseInitial): healthy accept + changed-version reject
        (referenceVerdict 0 100 0 0 dl tMax === validatorVerdict (mkContext 0 100 0 0 dl tMax))
          .&&. (referenceVerdict 1 100 0 0 dl tMax === validatorVerdict (mkContext 1 100 0 0 dl tMax))
          -- increment: version-bump accept + no-bump reject
          .&&. (incRef 1 0 === incVal (incRedeemer 3) 1 0)
          .&&. (incRef 0 0 === incVal (incRedeemer 3) 0 0)
          -- decrement: accept + value-perturbation reject
          .&&. (decRef 1 0 === decVal (decRedeemer 3) 1 0)
          .&&. (decRef 1 1_000 === decVal (decRedeemer 3) 1 1_000)
          -- contest: accept + too-old-snapshot reject
          .&&. (contestRef 1 0 1_500 === contestVal (contestRedeemer 1) 1 0 1_500)
          .&&. (contestRef 0 0 1_500 === contestVal (contestRedeemer 0) 0 0 1_500)
          -- close (CloseAny): snapshot > 0 accept + snapshot-0 reject
          .&&. (anyRef 0 100 3 0 dl tMax === anyVal (anyRedeemer 3) 0 100 3 0 dl tMax)
          .&&. (anyRef 0 100 0 0 dl tMax === anyVal (anyRedeemer 0) 0 100 0 0 dl tMax)
          -- close (CloseUsed, signature at version - 1): version-preserved accept + changed-version reject
          .&&. (usedRef usedOpenVersionN 100 3 0 dl tMax === usedVal (usedRedeemer 3) usedOpenVersionN 100 3 0 dl tMax)
          .&&. (usedRef 0 100 3 0 dl tMax === usedVal (usedRedeemer 3) 0 100 3 0 dl tMax)
          -- contest (ContestUsed, signature at version - 1): accept + too-old-snapshot reject
          .&&. (contestUsedRef 1 0 1_500 === contestUsedVal (contestUsedRedeemer 1) 1 0 1_500)
          .&&. (contestUsedRef 0 0 1_500 === contestUsedVal (contestUsedRedeemer 0) 0 0 1_500)
          -- contest deadline-push (n = 2): pushed-deadline accept + non-pushed reject
          .&&. (contest2Ref 1 openCpMs 1_500 === contest2Val (contest2Redeemer 1) 1 openCpMs 1_500)
          .&&. (contest2Ref 1 0 1_500 === contest2Val (contest2Redeemer 1) 1 0 1_500)
          -- init (μHead): healthy accept + wrong-mint-count reject
          .&&. (initRef 2 1 1 === initVal 2 1 1)
          .&&. (initRef 3 1 1 === initVal 3 1 1)
          -- recover (νDeposit, real Aiken UPLC): after-deadline accept + not-after reject
          .&&. (recoverRef 1_000 1_050 === recoverVal 1_000 1_050)
          .&&. (recoverRef 1_000 950 === recoverVal 1_000 950)
          -- claim (νDeposit, real Aiken UPLC): before-deadline accept + own-head-mismatch reject
          .&&. (claimRef 1_000 950 headPolicy === claimVal 1_000 950 headPolicy)
          .&&. (claimRef 1_000 950 otherHeadCid === claimVal 1_000 950 otherHeadCid)
          -- full fanout (real BLS): burn-count accept + wrong-count reject
          .&&. (fanoutRef 2 1_050 1_000 === fanoutVal 2 1_050 1_000)
          .&&. (fanoutRef 3 1_050 1_000 === fanoutVal 3 1_050 1_000)
          -- partial fanout (real KZG): 0<m accept + m=0 reject
          .&&. (partialRef 1 1_000 1_050 === partialVal 1 1_050 1_000)
          .&&. (partialRef 0 1_000 1_050 === partialVal 0 1_050 1_000)
          -- the C3 pulled-out conjuncts (value preservation, contest params, init head-id)
          .&&. (closeValueVal 2_000_000 === True)
          .&&. (closeValueVal 1_500_000 === False)
          .&&. (contestParamsVal (contestNext 1 0) === True)
          .&&. (contestParamsVal contestNextBadHeadId === False)
          .&&. (initHeadIdVal initOpenDatum === True)
          .&&. (initHeadIdVal initOpenDatumBadHeadId === False)
          -- the ref-spent conjunct (increment claimedDepositIsSpent): spent accept + unspent reject
          .&&. (Ref.checkRefSpent (encodeTxOutRef depRef) incInputRefCodes === True)
          .&&. (incVal (incRedeemer 3) 1 0 === True)
          .&&. (Ref.checkRefSpent (encodeTxOutRef unspentDepRef) incInputRefCodes === False)
          .&&. (incVal (incRedeemerUnspent 3) 1 0 === False)
