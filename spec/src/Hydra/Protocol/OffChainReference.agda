-- Executable, decidable reference for the OFF-CHAIN HeadLogic decisions (the §6 figure).
--
-- Off-chain analog of `Reference.agda`: kept self-contained over `Agda.Builtin` types so MAlonzo
-- extracts it to clean Haskell, which an off-chain differential test runs as a second oracle against
-- `Hydra.HeadLogic`. (See `Reference.agda`'s header for why the imports stay minimal: Prelude/OffChain
-- and stdlib do not extract / balloon the generated tree.)
--
-- Decisions covered (off-chain differential, step 5): the deposit-status transition of the §6 `tick`
-- handler, and the reqSn signing-eligibility check. The module grows one decidable decision per handler
-- guard as the off-chain mechanization proceeds.
module Hydra.Protocol.OffChainReference where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List

-- ── boundary types, bound to clean Haskell at extraction ──────────────────────────────────

-- Deposit lifecycle status (§6 `depositObj.status` / the 𝒟 registry).
data DepositStatusᶜ : Set where
  inactiveᶜ activeᶜ expiredᶜ : DepositStatusᶜ
{-# FOREIGN GHC data HsDepositStatus = InactiveS | ActiveS | ExpiredS deriving (Eq, Show) #-}
{-# COMPILE GHC DepositStatusᶜ = data HsDepositStatus (InactiveS | ActiveS | ExpiredS) #-}

-- ── small helper ──────────────────────────────────────────────────────────────────────────

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then x else _ = x
if false then _ else y = y

-- ── the decidable decision ──────────────────────────────────────────────────────────────────

-- The §6 `tick` deposit-status decision for one deposit object, as a pure function of times (all
-- POSIXTime; Nat truncated subtraction, matching the on-chain Reference's time arithmetic):
--   on (tick, t):  if  t > deadline − T_deposit   then Expired
--                  else if t > created + T_deposit then Active
--                  else Inactive (unchanged)
-- (`_<_`/`_-_`/`_+_` are the `Agda.Builtin.Nat` Bool-valued / arithmetic builtins; `t > x` is `x < t`.)
depositStatusRef : Nat → Nat → Nat → Nat → DepositStatusᶜ
depositStatusRef created deadline tDeposit t =
  if (deadline - tDeposit) < t then expiredᶜ
  else if (created + tDeposit) < t then activeᶜ
  else inactiveᶜ

infixr 6 _&&_
_&&_ : Bool → Bool → Bool
true  && b = b
false && _ = false

-- The §6 reqSn signing-eligibility check (`require v = v̂ ∧ s = ŝ + 1 ∧ leader(s) = j`), as a pure
-- decision over the version/number inputs with the leader test resolved to a Bool by the caller
-- (the leader function needs the party set, supplied Haskell-side). Mirrors the node's
-- `onOpenNetworkReqSn` `requireReqSn` (sv == version, sn == seenSn + 1, isLeader). `_==_` is the
-- `Agda.Builtin.Nat` Bool equality.
signEligibleRef : Nat → Nat → Nat → Nat → Bool → Bool
signEligibleRef v vHat s sHat leaderOk = (v == vHat) && ((s == suc sHat) && leaderOk)

infixr 5 _||_
_||_ : Bool → Bool → Bool
true  || _ = true
false || b = b

not : Bool → Bool
not true  = false
not false = true

-- List membership for the signature-accumulator (Σ̂) checks.
elemᵇ : Nat → List Nat → Bool
elemᵇ _ []       = false
elemᵇ x (y ∷ ys) = (x == y) || elemᵇ x ys

-- reqDec eligibility (the §6 `wait U_α = ∅ ∧ tx_ω = ⊥`): neither a commit nor a decommit in flight.
reqDecEligibleRef : Bool → Bool → Bool   -- (commitInFlight, decommitInFlight)
reqDecEligibleRef commit decommit = not commit && not decommit

-- ackSn-collect (the §6 `require (j,·) ∉ Σ̂`): sender j has not already signed this round.
notAlreadySignedRef : List Nat → Nat → Bool   -- (Σ̂ signer indices, j)
notAlreadySignedRef signers j = not (elemᵇ j signers)

-- ackSn-confirm (the §6 `if ∀ k ∈ [1..n] : (k,·) ∈ Σ̂`): every party index below n has signed (n-of-n).
allBelowᵇ : Nat → List Nat → Bool
allBelowᵇ zero    _       = true
allBelowᵇ (suc k) signers = elemᵇ k signers && allBelowᵇ k signers
allSignedRef : Nat → List Nat → Bool     -- (n, Σ̂ signer indices)
allSignedRef n signers = allBelowᵇ n signers

-- Contest re-post (the §6 `if S̄.s > s_c`): our confirmed snapshot is newer than the closed/contested one.
contestEligibleRef : Nat → Nat → Bool    -- (S̄.s, s_c)
contestEligibleRef sBar sc = sc < sBar

modSuc : Nat → Nat → Nat   -- modSuc a b = a mod (suc b), via the Agda.Builtin `mod-helper` primitive
modSuc a b = mod-helper 0 b a b

-- Round-robin leader, matching the node's `isLeader` (HeadLogic.hs): for `suc m` parties and snapshot
-- number sn, the party at 0-based index i is the leader iff (sn - 1) mod (suc m) == i. Only meaningful
-- for sn ≥ 1; the differential against the REAL `isLeader` exercises that domain.
leaderRef : Nat → Nat → Nat → Bool   -- (m where #parties = suc m, sn, party index i)
leaderRef m sn i = modSuc (sn - 1) m == i
