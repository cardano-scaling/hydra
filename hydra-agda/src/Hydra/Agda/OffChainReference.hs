-- | Clean Haskell surface over the MAlonzo-extracted OFF-CHAIN HeadLogic reference decisions
-- (@spec\/src\/Hydra\/Protocol\/OffChainReference.agda@). This is the off-chain analog of
-- "Hydra.Agda.Reference": the extractable half of the figure↔Agda↔Haskell correspondence for the §6
-- off-chain protocol, run as a second oracle against "Hydra.HeadLogic" in an off-chain differential
-- test. The mangled MAlonzo names below are pinned to the committed @generated/@ tree; if they change,
-- @regenerate.sh@ updates both this shim and @generated/@ together and a stale name is a compile error.
module Hydra.Agda.OffChainReference (
  -- * deposit lifecycle (the @tick@ handler)
  HsDepositStatus (..),
  depositStatusRef,

  -- * reqSn signing eligibility
  signEligibleRef,

  -- * reqDec / ackSn / contest guards
  reqDecEligibleRef,
  notAlreadySignedRef,
  allSignedRef,
  contestEligibleRef,

  -- * round-robin leader (bound to the real 'Hydra.HeadLogic.isLeader')
  leaderRef,
) where

import MAlonzo.Code.Hydra.Protocol.OffChainReference (HsDepositStatus (..))
import MAlonzo.Code.Hydra.Protocol.OffChainReference qualified as M

-- | Extracted decidable deposit-status decision of the §6 @tick@ handler: given a deposit's
-- @created@ and @deadline@ times, the deposit period @T_deposit@, and the current time @t@ (all
-- POSIXTime), it returns the deposit's lifecycle status. Mirrors the figure's
-- @on (tick, t)@ status transition (@t > deadline − T_deposit@ ⇒ Expired; else
-- @t > created + T_deposit@ ⇒ Active; else Inactive).
depositStatusRef :: Integer -> Integer -> Integer -> Integer -> HsDepositStatus
depositStatusRef = M.d_depositStatusRef_22

-- | Extracted decidable reqSn signing-eligibility check (the §6 @require v = v̂ ∧ s = ŝ + 1 ∧
-- leader(s) = j@): given the requested version @v@, the party's seen version @v̂@, the requested
-- snapshot number @s@, the party's seen number @ŝ@, and whether the requested leader is the sender
-- (resolved Haskell-side from the party set), decides whether the party should sign. Mirrors the node's
-- @onOpenNetworkReqSn@ @requireReqSn@.
signEligibleRef :: Integer -> Integer -> Integer -> Integer -> Bool -> Bool
signEligibleRef = M.d_signEligibleRef_36

-- | Extracted reqDec eligibility (§6 @wait U_α = ∅ ∧ tx_ω = ⊥@): given whether a commit and a decommit
-- are in flight, decides whether a new decommit may start.
reqDecEligibleRef :: Bool -> Bool -> Bool
reqDecEligibleRef = M.d_reqDecEligibleRef_62

-- | Extracted ackSn-collect guard (§6 @require (j,·) ∉ Σ̂@): given the signer indices already in Σ̂ and a
-- sender @j@, decides whether @j@ is a fresh signer.
notAlreadySignedRef :: [Integer] -> Integer -> Bool
notAlreadySignedRef = M.d_notAlreadySignedRef_68

-- | Extracted ackSn-confirm guard (§6 @if ∀ k ∈ [1..n] : (k,·) ∈ Σ̂@): given the party count @n@ and the
-- signer indices in Σ̂, decides whether every party (index @< n@) has signed (n-of-n).
allSignedRef :: Integer -> [Integer] -> Bool
allSignedRef = M.d_allSignedRef_80

-- | Extracted contest re-post guard (§6 @if S̄.s > s_c@): our confirmed snapshot number vs the
-- closed/contested one.
contestEligibleRef :: Integer -> Integer -> Bool
contestEligibleRef = M.d_contestEligibleRef_86

-- | Extracted round-robin leader (the §6 @leader(s)@). Arguments: @m@ where the head has @suc m@ parties,
-- the snapshot number, and a 0-based party index. Bound against the real 'Hydra.HeadLogic.isLeader' in
-- the hydra-node off-chain differential.
leaderRef :: Integer -> Integer -> Integer -> Bool
leaderRef = M.d_leaderRef_98
