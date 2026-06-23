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
