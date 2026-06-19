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
-- NB: must NOT import Prelude/OnChain/abstract-set-theory — those do not extract.
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
{-# FOREIGN GHC data HsClosed = MkClosed Integer Integer Integer Integer #-}
{-# COMPILE GHC Closedᶜ = data HsClosed (MkClosed) #-}

-- Injected operations: the conjuncts the decidable layer does not model — crypto/value/
-- accumulator AND, for now, the deadline / bounded-validity checks (they need the tx validity
-- range + POSIXTime/period unit handling; deferred to a follow-up). Supplied as a Haskell
-- function (mocked = const True in the differential test).
record Ops : Set where
  field
    closeCryptoOK : Openᶜ → Closedᶜ → CloseTagᶜ → Bool
open Ops public

-- ── small decidable helpers over Nat/Bool ─────────────────────────────────────────────────

infixr 6 _&&_
_&&_ : Bool → Bool → Bool
true  && b = b
false && _ = false

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

isNull : {A : Set} → List A → Bool
isNull []      = true
isNull (_ ∷ _) = false

-- ── the decidable close checker ───────────────────────────────────────────────────────────
-- Mirrors the decidable, unit-robust conjuncts of `closeValid` (OnChain.lagda.typ):
--   • version preserved (Open.v ≡ Closed.v) and contestation period preserved
--   • contesters initialised to [] (length 0)
--   • closeInitial ⇒ Open.v ≡ 0 ∧ Closed.s ≡ 0   (the η ≡ accUTxO ∅ part is in `Ops`)
--   • closeAny    ⇒ 0 < Closed.s
-- conjoined with the injected (mock) op (crypto/value + deadline/bounded-validity, deferred).
closeRefᵇ : Ops → Openᶜ → Closedᶜ → CloseTagᶜ → Bool
closeRefᵇ ops o c tag =
      (Openᶜ.versionO o ==ᵇ Closedᶜ.versionC c)
   && (Openᶜ.cpO o ==ᵇ Closedᶜ.cpC c)
   && (Closedᶜ.contesterLenC c ==ᵇ zero)
   && initialOK tag
   && anyOK tag
   && closeCryptoOK ops o c tag
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
-- the deposit (increment) lovelace. (The full multi-asset `Value` is not extractable; lovelace —
-- a plain Integer — is the boundary-friendly component the differential test can supply for real.)
record IncIOᶜ : Set where
  constructor mkIncIOᶜ
  field
    versionIn  : Nat
    versionOut : Nat
    adaIn      : Nat
    adaDelta   : Nat
    adaOut     : Nat
{-# FOREIGN GHC data HsIncIO = MkIncIO Integer Integer Integer Integer Integer #-}
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
  && incCryptoOK ops i

-- decrement: same transition shape (version bumps); its value conservation removes the decommit,
-- which the differential test cannot yet supply as an extractable lovelace independently, so the
-- decrement reference checks the version discipline only (value/crypto injected). Takes the same
-- boundary type; the ada fields are ignored here.
decRefᵇ : OpsInc → IncIOᶜ → Bool
decRefᵇ ops i =
  (IncIOᶜ.versionOut i ==ᵇ suc (IncIOᶜ.versionIn i)) && incCryptoOK ops i

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
{-# FOREIGN GHC data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer #-}
{-# COMPILE GHC ContestIOᶜ = data HsContestIO (MkContestIO) #-}

record OpsContest : Set where
  field contestCryptoOK : ContestIOᶜ → Bool
open OpsContest public

contestRefᵇ : OpsContest → ContestIOᶜ → Bool
contestRefᵇ ops c =
      (ContestIOᶜ.versionInK c ==ᵇ ContestIOᶜ.versionOutK c)
   && (ContestIOᶜ.snapIn c <ᵇ ContestIOᶜ.snapOut c)
   && (ContestIOᶜ.contesterLenOut c ==ᵇ suc (ContestIOᶜ.contesterLenIn c))
   && contestCryptoOK ops c

-- ══ fanout / finalPartialFanout ═══════════════════════════════════════════════════════════
-- The decidable conjunct of `fanoutValid`/`finalPartialFanoutValid` is `0 < m`, the
-- number of distributed outputs (the validator's `FanoutZeroOutputs`, the §5.8 m>0 fix).
-- Accumulator membership / value / burn / deadline are injected.
record Fanoutᶜ : Set where
  constructor mkFanoutᶜ
  field
    numOutputsF : Nat
{-# FOREIGN GHC data HsFanout = MkFanout Integer #-}
{-# COMPILE GHC Fanoutᶜ = data HsFanout (MkFanout) #-}

record OpsFanout : Set where
  field fanoutCryptoOK : Fanoutᶜ → Bool
open OpsFanout public

fanoutRefᵇ : OpsFanout → Fanoutᶜ → Bool
fanoutRefᵇ ops f =
  (zero <ᵇ Fanoutᶜ.numOutputsF f) && fanoutCryptoOK ops f
