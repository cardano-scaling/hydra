-- Executable, decidable reference checker for the on-chain validator conditions.
--
-- This module is the EXTRACTABLE half of the Agda↔Haskell correspondence (Tier 2): it is
-- self-contained over concrete `Agda.Builtin` types (Nat→Integer, Bool→Bool, List→[]) and
-- carries `FOREIGN`/`COMPILE GHC` bindings so MAlonzo extracts it to clean Haskell, which the
-- `hydra-tx` differential test runs as a second oracle alongside the real Plutus validator.
--
-- It mirrors only the DECIDABLE conjuncts of the validity bundles in `OnChain.lagda.typ`
-- (state-machine shape, version discipline, deadline/bounds arithmetic, snapshot ordering,
-- contester checks). The crypto/value/accumulator conjuncts are lumped into an injected `Ops`
-- record, supplied (mocked) from Haskell. Correspondence to the abstract `closeValid` is proved
-- (separately, typecheck-only) in `ReferenceBridge.lagda.typ`.
--
-- NB on imports: this module stays SELF-CONTAINED over `Agda.Builtin.{Bool,Nat,List}`. Two reasons:
-- (1) Prelude/OnChain/abstract-set-theory do not extract at all; (2) even stdlib modules that DO
-- extract balloon the committed `generated/MAlonzo` tree - importing `Data.Bool.Base` just for `not`
-- pulls in `Level`/`Data.Empty`/`Data.Irrelevant`/`Data.Unit.Base` and grows the tree from 7 to 13
-- generated files (measured). So the few small Bool/Nat helpers below are hand-rolled on purpose; the
-- structural `_==ᵇ_`/`_≤ᵇ_`/`_<ᵇ_` additionally let their reflection lemmas be PROVED (not postulated)
-- in `RefReflection`. The PROOF-side modules (RefReflection, ReferenceBridge) are typecheck-only and
-- import stdlib freely - minimality only matters here, on the extracted side.
module Hydra.Protocol.Reference where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List

-- ── concrete boundary types, bound to clean Haskell types at extraction ───────────────────

-- Close redeemer selector (the CloseType union, payload-free at this layer).
data CloseTagᶜ : Set where
  closeInitialᶜ closeAnyᶜ closeUnusedᶜ closeUsedᶜ : CloseTagᶜ
{-# FOREIGN GHC data HsCloseTag = CloseInitialT | CloseAnyT | CloseUnusedT | CloseUsedT #-}
{-# COMPILE GHC CloseTagᶜ = data HsCloseTag (CloseInitialT | CloseAnyT | CloseUnusedT | CloseUsedT) #-}

-- The fields of the input (Open) datum the decidable close checks read.
record Openᶜ : Set where
  constructor mkOpenᶜ
  field
    versionO : Nat
    cpO      : Nat
{-# FOREIGN GHC data HsOpen = MkOpen Integer Integer #-}
{-# COMPILE GHC Openᶜ = data HsOpen (MkOpen) #-}

-- The fields of the produced (Closed) datum the decidable close checks read.
record Closedᶜ : Set where
  constructor mkClosedᶜ
  field
    versionC      : Nat
    cpC           : Nat
    snapshotC     : Nat
    contesterLenC : Nat
    tfinalC       : Nat   -- the recorded contestation deadline (POSIXTime ms)
{-# FOREIGN GHC data HsClosed = MkClosed Integer Integer Integer Integer Integer #-}
{-# COMPILE GHC Closedᶜ = data HsClosed (MkClosed) #-}

-- Injected operations: the conjuncts the decidable layer does not model - crypto/value/
-- accumulator. Supplied as a Haskell function (mocked = const True in the differential test).
record Ops : Set where
  field
    closeCryptoOK : Openᶜ → Closedᶜ → CloseTagᶜ → Bool
open Ops public

-- ── small decidable helpers over Nat/Bool ─────────────────────────────────────────────────

infixr 6 _&&_
_&&_ : Bool → Bool → Bool
true  && b = b
false && _ = false

infixr 5 _||_
_||_ : Bool → Bool → Bool
true  || _ = true
false || b = b

_==ᵇ_ : Nat → Nat → Bool
zero    ==ᵇ zero    = true
zero    ==ᵇ (suc _) = false
(suc _) ==ᵇ zero    = false
(suc m) ==ᵇ (suc n) = m ==ᵇ n

_≤ᵇ_ : Nat → Nat → Bool
zero    ≤ᵇ _       = true
(suc _) ≤ᵇ zero    = false
(suc m) ≤ᵇ (suc n) = m ≤ᵇ n

_<ᵇ_ : Nat → Nat → Bool
m <ᵇ n = (suc m) ≤ᵇ n

-- builtin-based ≤ (native at extraction): `a ≤ b ⟺ a < b + 1`, on the BUILTIN strict `_<_` and `suc`
-- (no extra import, no hand-rolled `not`). Used for the "posted before the deadline" conjuncts
-- (`hi ≤ tfinal` / `hi ≤ tRecover`), whose POSIXTime-ms operands are far too large for the structural
-- `_≤ᵇ_` (O(n) unary recursion). Its reflection lemma is PROVED (not postulated) from `<ᴮ-sound`.
_≤ᴮ_ : Nat → Nat → Bool
a ≤ᴮ b = a < suc b

-- Boolean conditional (hand-rolled to keep the extractable module self-contained over Agda.Builtin;
-- used for the contest conditional deadline-update rule). Extracts to a clean GHC `case`.
if_then_else_ : {A : Set} → Bool → A → A → A
if true  then a else _ = a
if false then _ else b = b

-- ── the decidable close checker ───────────────────────────────────────────────────────────
-- Mirrors the decidable, unit-robust conjuncts of `closeValid` (OnChain.lagda.typ):
--   • version preserved (Open.v ≡ Closed.v) and contestation period preserved
--   • contesters initialised to [] (length 0)
--   • closeInitial ⇒ Open.v ≡ 0 ∧ Closed.s ≡ 0   (the η ≡ accUTxO ∅ part is in `Ops`)
--   • closeAny    ⇒ 0 < Closed.s
--   • the recorded deadline is the tx upper validity bound + the contestation period: tfinal ≡
--     validityHi + cp (`closeDeadlineOK`, Plutus `checkDeadline`/`makeContestationDeadline`). Uses the
--     BUILTIN `_==_` (native Integer eq) and `_+_`: the values are POSIXTime ms, far too large for the
--     structural `_==ᵇ_` (which is O(n) unary recursion). `validityHi` is the tx upper bound in ms.
--   • the validity range is bounded so the deadline is at most one period ahead: `hi ∸ lo ≤ cp`
--     (`validityBounded`, §5.6); uses the BUILTIN truncated `_-_` and `_≤ᴮ_` (POSIXTime ms). `validityLo`
--     is the tx LOWER bound in ms.
-- The remaining crypto/value conjuncts are injected (mock).
closeRefᵇ : Ops → Openᶜ → Closedᶜ → CloseTagᶜ → Nat → Nat → Bool
closeRefᵇ ops o c tag validityHi validityLo =
      (Openᶜ.versionO o ==ᵇ Closedᶜ.versionC c)
   && (Openᶜ.cpO o ==ᵇ Closedᶜ.cpC c)
   && (Closedᶜ.contesterLenC c ==ᵇ zero)
   && initialOK tag
   && anyOK tag
   && closeCryptoOK ops o c tag
   && (Closedᶜ.tfinalC c == (validityHi + Openᶜ.cpO o))
   && ((validityHi - validityLo) ≤ᴮ Openᶜ.cpO o)
  where
    initialOK : CloseTagᶜ → Bool
    initialOK closeInitialᶜ = (Openᶜ.versionO o ==ᵇ zero) && (Closedᶜ.snapshotC c ==ᵇ zero)
    initialOK _             = true
    anyOK : CloseTagᶜ → Bool
    anyOK closeAnyᶜ = zero <ᵇ Closedᶜ.snapshotC c
    anyOK _         = true

-- ══ increment / decrement ═════════════════════════════════════════════════════════════════
-- The single decidable conjunct of `incrementValid`/`decrementValid` is the version
-- discipline: the produced Open datum carries `suc v` (transition `Open … v … ⟶ Open … (suc v)`),
-- which the validator enforces as `VersionNotIncremented`. Crypto/value are injected.

-- The version fields of the input/produced Open datums, plus the lovelace (ada) amounts the
-- value-conservation check needs: `adaIn`/`adaOut` are the head input/output lovelace, `adaDelta`
-- the deposit (increment) lovelace. (The full multi-asset `Value` is not extractable; lovelace -
-- a plain Integer - is the boundary-friendly component the differential test can supply for real.)
record IncIOᶜ : Set where
  constructor mkIncIOᶜ
  field
    versionIn  : Nat
    versionOut : Nat
    adaIn      : Nat
    adaDelta   : Nat
    adaOut     : Nat
    nonAdaIn   : Nat   -- total NON-ada token quantity of the head input  (`nonAdaOf headValueIn`)
    nonAdaDelta : Nat  -- total non-ada quantity of the deposit / decommit (`nonAdaOf depositsValue` / `decommitValue`)
    nonAdaOut  : Nat   -- total non-ada token quantity of the head output (`nonAdaOf headValue`)
{-# FOREIGN GHC data HsIncIO = MkIncIO Integer Integer Integer Integer Integer Integer Integer Integer #-}
{-# COMPILE GHC IncIOᶜ = data HsIncIO (MkIncIO) #-}

record OpsInc : Set where
  field incCryptoOK : IncIOᶜ → Bool
open OpsInc public

-- increment: version bumps (`VersionNotIncremented`) AND head value grows by the deposit
-- (`mustPreserveValue`): on the lovelace component, adaIn + adaDelta ≡ adaOut. Crypto injected.
-- The lovelace equality uses the BUILTIN `_==_` (extracts to native Integer equality): the
-- structural `_==ᵇ_` is O(n) unary recursion, which is pathological on lovelace-scale values
-- (millions), so it must NOT be used here. The version bump stays on `_==ᵇ_` (versions are small).
incRefᵇ : OpsInc → IncIOᶜ → Bool
incRefᵇ ops i =
     (IncIOᶜ.versionOut i ==ᵇ suc (IncIOᶜ.versionIn i))
  && ((IncIOᶜ.adaIn i + IncIOᶜ.adaDelta i) == IncIOᶜ.adaOut i)
  && ((IncIOᶜ.nonAdaIn i + IncIOᶜ.nonAdaDelta i) == IncIOᶜ.nonAdaOut i)
  && incCryptoOK ops i

-- decrement: same transition shape (version bumps) AND head value SHRINKS by the decommit
-- (`mustDecreaseValue`): on the lovelace component, adaOut + adaDelta ≡ adaIn (head output + the
-- decommitted outputs ≡ head input). Note the equation differs from increment's (which grows): here
-- the deposit field `adaDelta` carries the decommit lovelace and the head INPUT is the larger side.
-- Uses the BUILTIN `_==_` (native Integer equality) on the lovelace, as in `incRefᵇ`. Crypto injected.
decRefᵇ : OpsInc → IncIOᶜ → Bool
decRefᵇ ops i =
     (IncIOᶜ.versionOut i ==ᵇ suc (IncIOᶜ.versionIn i))
  && ((IncIOᶜ.adaOut i + IncIOᶜ.adaDelta i) == IncIOᶜ.adaIn i)
  && ((IncIOᶜ.nonAdaOut i + IncIOᶜ.nonAdaDelta i) == IncIOᶜ.nonAdaIn i)
  && incCryptoOK ops i

-- ══ contest ═══════════════════════════════════════════════════════════════════════════════
-- Decidable conjuncts of `contestValid` (transition `Closed … v s … C … ⟶ Closed … v s' … (kh ∷ C)`):
--   • version preserved (vIn ≡ vOut)
--   • snapshot strictly increases (sIn < sOut), the validator's `TooOldSnapshot`
--   • exactly one contester appended (contesterLenOut ≡ suc contesterLenIn)
-- Crypto/value/deadline are injected.
record ContestIOᶜ : Set where
  constructor mkContestIOᶜ
  field
    versionInK      : Nat
    versionOutK     : Nat
    snapIn          : Nat
    snapOut         : Nat
    contesterLenIn  : Nat
    contesterLenOut : Nat
    tfinalK         : Nat   -- the (input) recorded contestation deadline (POSIXTime ms)
    validityHiK     : Nat   -- the contest tx's upper validity bound (POSIXTime ms)
    tfinalOutK      : Nat   -- the PRODUCED datum's recorded deadline tfinal' (POSIXTime ms)
    numPartiesK     : Nat   -- n: the number of parties (from the head datum)
    cpK             : Nat   -- the contestation period T (added when not all parties have contested)
{-# FOREIGN GHC data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer #-}
{-# COMPILE GHC ContestIOᶜ = data HsContestIO (MkContestIO) #-}

record OpsContest : Set where
  field contestCryptoOK : ContestIOᶜ → Bool
open OpsContest public

-- Decidable contest conjuncts: version preserved, snapshot strictly increases, one contester appended,
-- posted before the deadline (`validityHi ≤ᴮ tfinal`), AND the conditional deadline-UPDATE rule (§5.7,
-- Plutus `makeContestationDeadline`): tfinal' = tfinal if EVERY party has now contested
-- (contesterLenOut ≡ n), else tfinal + cp. The count test uses the structural `_==ᵇ_` (small n); the
-- deadline arithmetic/equality use the BUILTIN `_+_`/`_==_` (POSIXTime ms). Crypto/value injected.
contestRefᵇ : OpsContest → ContestIOᶜ → Bool
contestRefᵇ ops c =
      (ContestIOᶜ.versionInK c ==ᵇ ContestIOᶜ.versionOutK c)
   && (ContestIOᶜ.snapIn c <ᵇ ContestIOᶜ.snapOut c)
   && (ContestIOᶜ.contesterLenOut c ==ᵇ suc (ContestIOᶜ.contesterLenIn c))
   && (ContestIOᶜ.validityHiK c ≤ᴮ ContestIOᶜ.tfinalK c)
   && (ContestIOᶜ.tfinalOutK c ==
        (if (ContestIOᶜ.contesterLenOut c ==ᵇ ContestIOᶜ.numPartiesK c)
         then ContestIOᶜ.tfinalK c
         else (ContestIOᶜ.tfinalK c + ContestIOᶜ.cpK c)))
   && contestCryptoOK ops c

-- ══ fanout / finalPartialFanout ═══════════════════════════════════════════════════════════
-- The decidable conjuncts of `fanoutValid`/`finalPartialFanoutValid`:
--   • all `n+1` head tokens burned (`burnAllTokensOK`: `burnedCount == n+1`, the mirror of the init
--     mint count) - BUILTIN `_==_` (a mutation could inject a large burn quantity);
--   • posted strictly AFTER the deadline (`tfinal < lo`, the mirror of the recover after-deadline) -
--     BUILTIN `_<_` (POSIXTime ms). Accumulator membership / value conservation are injected.
-- NB no `0 < m` conjunct: the FULL fanout permits m = 0 (finalising an empty head), so the reference
-- must not reject it (it would contradict the relaxed νHead `headIsFinalizedWith`). The `numOutputsF`
-- field is supplied for the differential but not gated. (Partial fanout's `0 < m` is enforced
-- by its own validator guard, not modelled at this shared checker.)
record Fanoutᶜ : Set where
  constructor mkFanoutᶜ
  field
    numOutputsF  : Nat   -- m: number of distributed outputs
    burnedCountF : Nat   -- total head-policy tokens burned (negated mint quantity)
    numPartiesF  : Nat   -- n (from the head datum)
    tfinalF      : Nat   -- the recorded contestation deadline (POSIXTime ms)
    validityLoF  : Nat   -- the fanout tx's lower validity bound (POSIXTime ms)
{-# FOREIGN GHC data HsFanout = MkFanout Integer Integer Integer Integer Integer #-}
{-# COMPILE GHC Fanoutᶜ = data HsFanout (MkFanout) #-}

record OpsFanout : Set where
  field fanoutCryptoOK : Fanoutᶜ → Bool
open OpsFanout public

fanoutRefᵇ : OpsFanout → Fanoutᶜ → Bool
fanoutRefᵇ ops f =
     (Fanoutᶜ.burnedCountF f == suc (Fanoutᶜ.numPartiesF f))
  && (Fanoutᶜ.tfinalF f < Fanoutᶜ.validityLoF f)
  && fanoutCryptoOK ops f

-- ══ deposit recover (νDeposit, Recover redeemer) ══════════════════════════════════════════════
-- The decidable conjunct of `recoverValid` (deposit.ak's Recover arm, §5.3.2): the recover tx is
-- posted strictly AFTER the recover deadline - txValidityMin > tRecover, i.e. `tRecover < lo`. Uses
-- the BUILTIN `_<_` (native Integer `<` at extraction): the deadline is a POSIXTime in milliseconds,
-- far too large for the structural `_<ᵇ_` (O(n) unary recursion), exactly as on the close deadline.
-- The recovered-outputs hash equality (deposit.ak `recover_outputs`, the serialisation-hash match) is
-- crypto/serialisation and is injected (mock), as the close/inc crypto conjuncts are.
record RecoverIOᶜ : Set where
  constructor mkRecoverIOᶜ
  field
    tRecoverR   : Nat   -- the deposit datum's recover deadline (POSIXTime ms)
    validityLoR : Nat   -- the recover tx's lower validity bound (POSIXTime ms)
{-# FOREIGN GHC data HsRecoverIO = MkRecoverIO Integer Integer #-}
{-# COMPILE GHC RecoverIOᶜ = data HsRecoverIO (MkRecoverIO) #-}

record OpsRecover : Set where
  field recoverHashOK : RecoverIOᶜ → Bool
open OpsRecover public

recoverRefᵇ : OpsRecover → RecoverIOᶜ → Bool
recoverRefᵇ ops r =
  (RecoverIOᶜ.tRecoverR r < RecoverIOᶜ.validityLoR r) && recoverHashOK ops r

-- ══ init (μHead minting policy: token COUNT + PLACEMENT) ══════════════════════════════════════
-- The decidable conjuncts of `initValid` / the μHead policy (`HeadTokens.validateTokensMinting`):
--   • the tx MINTS exactly `n + 1` tokens of the head policy (one ST + one PT per party,
--     `checkNumberOfTokens`: `mintedTokenCount == nParties + 1`) - `mintedCountM`;
--   • the n+1 tokens are PLACED in the head output: the ST is present (`stQtyM == 1`) AND the head
--     output carries exactly n+1 head-policy tokens (`headTokenCountM == n+1`). Mint count + placed
--     count together pin that every minted token lands in the head output (the value-map API the spec
--     abstracts over). All BUILTIN `_==_` (counts are small but a mutation could inject a
--     large quantity). The remaining μHead checks - seed-input spent and the datum `headId`/`seed`
--     binding - stay injected (mock); a hand-reviewed / type-encoded boundary. (Naming the individual
--     PTs is out of reach: the head datum abstracts the per-party keys into `hk`/`n`.)
record MintIOᶜ : Set where
  constructor mkMintIOᶜ
  field
    numPartiesM     : Nat   -- n: the number of parties (from the head datum)
    mintedCountM    : Nat   -- the head policy's total minted token quantity
    stQtyM          : Nat   -- quantity of the ST in the head output (should be 1)
    headTokenCountM : Nat   -- number of head-policy tokens in the head output (should be n+1)
{-# FOREIGN GHC data HsMintIO = MkMintIO Integer Integer Integer Integer #-}
{-# COMPILE GHC MintIOᶜ = data HsMintIO (MkMintIO) #-}

record OpsInit : Set where
  field initPlacementOK : MintIOᶜ → Bool
open OpsInit public

initRefᵇ : OpsInit → MintIOᶜ → Bool
initRefᵇ ops m =
     (MintIOᶜ.mintedCountM m == suc (MintIOᶜ.numPartiesM m))
  && (MintIOᶜ.stQtyM m == 1)
  && (MintIOᶜ.headTokenCountM m == suc (MintIOᶜ.numPartiesM m))
  && initPlacementOK ops m

-- ══ deposit claim (νDeposit, Claim redeemer) ══════════════════════════════════════════════════
-- The decidable conjunct of `claimValid` (deposit.ak's Claim arm, §5.2): the increment tx collecting
-- the deposit is posted BEFORE the recover deadline - txValidityMax ≤ tRecover, i.e.
-- `validityHi ≤ᴮ tRecover` (BUILTIN-based `_≤ᴮ_`, POSIXTime ms), AND the own-head binding
-- (`depositClaimedBy`, deposit.ak `expect_increment_redeemer`): the deposit datum's head id equals the
-- head being spent. Head ids are hashes; the boundary represents each as the Integer `depositCidC` /
-- `headCidC` (a deterministic encoding supplied by the test) and checks equality with the BUILTIN `_==_`
-- (native Integer eq; the encodings may be large). The Increment-redeemer coupling stays injected.
record ClaimIOᶜ : Set where
  constructor mkClaimIOᶜ
  field
    tRecoverC   : Nat   -- the deposit datum's recover deadline (POSIXTime ms)
    validityHiC : Nat   -- the claim (increment) tx's upper validity bound (POSIXTime ms)
    depositCidC : Nat   -- the deposit datum's head id, encoded as an Integer
    headCidC    : Nat   -- the spent head's id, encoded as an Integer
{-# FOREIGN GHC data HsClaimIO = MkClaimIO Integer Integer Integer Integer #-}
{-# COMPILE GHC ClaimIOᶜ = data HsClaimIO (MkClaimIO) #-}

record OpsClaim : Set where
  field claimIncrementOK : ClaimIOᶜ → Bool
open OpsClaim public

claimRefᵇ : OpsClaim → ClaimIOᶜ → Bool
claimRefᵇ ops c =
     (ClaimIOᶜ.validityHiC c ≤ᴮ ClaimIOᶜ.tRecoverC c)
  && (ClaimIOᶜ.depositCidC c == ClaimIOᶜ.headCidC c)
  && claimIncrementOK ops c

-- ══ participant signature (shared: close / contest / increment / decrement) ════════════════════
-- The §5.4–5.7 `mustBeSignedByParticipant` check, a fully-extractable conjunct of its own
-- (no injected Ops): SOME transaction signer holds a participation token in
-- the head value. Both sides are Integer-encoded key-hashes - `signerCodesS` the tx's signing key-hashes
-- (txInfoSignatories), `ptCodesS` the names of the participation tokens carried by the head value (a PT's
-- token name IS a participant's key-hash) - and the check is that the two lists OVERLAP. The differential
-- supplies both lists from the real tx with the SAME hash→Integer encoding, so a non-participant signer
-- (the validator's `SignerIsNotAParticipant`) makes the lists disjoint and the reference reject. Uses the
-- BUILTIN `_==_` (key-hash encodings are large).
elemᵇ : Nat → List Nat → Bool
elemᵇ _ []       = false
elemᵇ x (y ∷ ys) = (x == y) || elemᵇ x ys

anySharedᵇ : List Nat → List Nat → Bool
anySharedᵇ []       _  = false
anySharedᵇ (x ∷ xs) ys = elemᵇ x ys || anySharedᵇ xs ys

record SignerIOᶜ : Set where
  constructor mkSignerIOᶜ
  field
    signerCodesS : List Nat   -- Integer-encoded key-hashes of the tx signers (txInfoSignatories)
    ptCodesS     : List Nat   -- Integer-encoded names of the participation tokens in the head value
{-# FOREIGN GHC data HsSignerIO = MkSignerIO [Integer] [Integer] #-}
{-# COMPILE GHC SignerIOᶜ = data HsSignerIO (MkSignerIO) #-}

participantSignedRefᵇ : SignerIOᶜ → Bool
participantSignedRefᵇ s = anySharedᵇ (SignerIOᶜ.signerCodesS s) (SignerIOᶜ.ptCodesS s)

-- ══ per-asset value conservation (increment / decrement) ══════════════════════════════════════
-- The finer companion to the `adaOf`/`nonAdaOf` TOTALS checked by `incRefᵇ`/`decRefᵇ`. Given each native
-- asset's (qIn, qDelta, qOut) — its quantity in the head input, the delta (deposit on increment /
-- decommit on decrement) and the head output — EVERY asset must conserve: qIn + qDelta == qOut. (For the
-- decrement direction the caller supplies (qOut, qDelta, qIn) so the same sum-check applies.) Catches a
-- SELECTIVE single-token siphon that leaves the two scalar totals balanced. Each quantity is a
-- (non-negative) per-asset amount (`quantityOfᴺ`); BUILTIN `_==_` per asset (amounts may be large).
-- Defined last so MAlonzo appends fresh names without drifting the earlier mangled names in the shim.
record AssetIOᶜ : Set where
  constructor mkAssetIOᶜ
  field
    qInA    : Nat
    qDeltaA : Nat
    qOutA   : Nat
{-# FOREIGN GHC data HsAssetIO = MkAssetIO Integer Integer Integer #-}
{-# COMPILE GHC AssetIOᶜ = data HsAssetIO (MkAssetIO) #-}

perAssetConservedᵇ : List AssetIOᶜ → Bool
perAssetConservedᵇ []       = true
perAssetConservedᵇ (a ∷ as) =
  ((AssetIOᶜ.qInA a + AssetIOᶜ.qDeltaA a) == AssetIOᶜ.qOutA a) && perAssetConservedᵇ as
