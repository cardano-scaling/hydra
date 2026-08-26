-- | Clean Haskell surface over the MAlonzo-extracted OFF-CHAIN HeadLogic reference decisions
-- (@spec\/src\/Hydra\/Protocol\/OffChainReference.agda@). This is the off-chain analog of
-- "Hydra.Agda.Reference": the extractable half of the figure↔Agda↔Haskell correspondence for the §6
-- off-chain protocol, run as a second oracle against "Hydra.HeadLogic" in an off-chain differential
-- test. Each binding names an export the Agda source fixes with @COMPILE GHC … as …@, not a mangled
-- MAlonzo name, so regeneration after an unrelated Agda edit does not renumber them out from under
-- this shim (see the header of "Hydra.Agda.Reference").
--
-- As there, every 'Integer' stands for an Agda ℕ and callers must pass non-negative values; see that
-- module's header for what a negative one does.
module Hydra.Agda.OffChainReference (
  -- * deposit lifecycle (the @tick@ handler)
  HsDepositStatus (..),
  depositStatusRef,

  -- * reqSn signing eligibility
  signEligibleRef,

  -- * reqSn incremental-action guards
  reqSnNotBothRef,
  reqSnDecommitOutputsRef,
  reqSnDepositSettledRef,

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
-- @created@ and @deadline@ times, the expiry period @T_deposit@, the activation period
-- @T_activate@ and the current time @t@ (all POSIXTime), it returns the deposit's lifecycle status.
-- Mirrors the figure's @on (tick, t)@ status transition (@t > deadline - T_deposit@ ⇒ Expired; else
-- @t > created + T_activate@ ⇒ Active; else Inactive).
--
-- The two periods are separate arguments because the node configures them separately
-- (@--deposit-period@ bounds expiry, @--deposit-activation@ the Inactive to Active transition), so
-- an oracle taking one period could only agree with the node where the two happen to be equal.
depositStatusRef :: Integer -> Integer -> Integer -> Integer -> Integer -> HsDepositStatus
depositStatusRef = M.hsDepositStatusRef

-- | Extracted decidable reqSn signing-eligibility check (the §6 @require v = v̂ ∧ s = ŝ + 1 ∧
-- leader(s) = j@): given the requested version @v@, the party's seen version @v̂@, the requested
-- snapshot number @s@, the party's seen number @ŝ@, and whether the requested leader is the sender
-- (resolved Haskell-side from the party set), decides whether the party goes on to sign.
--
-- Only that decision, not the outcome a rejection produces. @onOpenNetworkReqSn@ splits the three
-- conjuncts: @requireReqSn@ errors on the number (@ReqSnNumberInvalid@) and the leader
-- (@ReqSnNotLeader@), while the version conjunct is @waitOnSnapshotVersion@, a wait, so that a
-- follower which has not yet processed the version-bumping increment\/decrement retries instead of
-- dropping the message. The §6 figure writes all three as @require@.
signEligibleRef :: Integer -> Integer -> Integer -> Integer -> Bool -> Bool
signEligibleRef = M.hsSignEligibleRef

-- | Extracted reqDec eligibility (§6 @wait U_α = ∅ ∧ tx_ω = ⊥@): given whether a commit and a decommit
-- are in flight, decides whether a new decommit may start.
reqDecEligibleRef :: Bool -> Bool -> Bool
reqDecEligibleRef = M.hsReqDecEligibleRef

-- | Extracted reqSn incremental-action exclusivity (§6 @require tx_ω = ⊥ ∨ tx_α = ⊥@, the node's
-- @ReqSnBothCommitAndDecommit@): given whether the request carries a deposit and whether it carries a
-- decommit, decides whether the combination is admissible (both at once is not).
reqSnNotBothRef :: Bool -> Bool -> Bool
reqSnNotBothRef = M.hsReqSnNotBothRef

-- | Extracted reqSn decommit-materializes-outputs guard (the node's @ReqSnDecommitNoOutputs@): given
-- the number of outputs the requested decommit produces, decides whether it could ever settle
-- on-chain (the decrement validator materializes the decommit outputs and requires at least one).
reqSnDecommitOutputsRef :: Integer -> Bool
reqSnDecommitOutputsRef = M.hsReqSnDecommitOutputsRef

-- | Extracted reqSn same-version deposit-settlement guard (the node's @waitForDeposit@ /
-- @ReqSnCommitNotSettled@ identity check): given whether the requested deposit's content matches the
-- confirmed snapshot's pending commit, and the Integer-encoded deposit tx-ids bound into the
-- confirmed snapshot and carried by the request, decides whether the request settles the same
-- deposit, by identity rather than content alone (two deposits can record the same UTxO).
reqSnDepositSettledRef :: Bool -> Integer -> Integer -> Bool
reqSnDepositSettledRef = M.hsReqSnDepositSettledRef

-- | Extracted ackSn-collect guard (§6 @require (j,·) ∉ Σ̂@): given the signer indices already in Σ̂ and a
-- sender @j@, decides whether @j@ is a fresh signer.
notAlreadySignedRef :: [Integer] -> Integer -> Bool
notAlreadySignedRef = M.hsNotAlreadySignedRef

-- | Extracted ackSn-confirm guard (§6 @if ∀ k ∈ [1..n] : (k,·) ∈ Σ̂@): given the party count @n@ and the
-- signer indices in Σ̂, decides whether every party (index @< n@) has signed (n-of-n).
allSignedRef :: Integer -> [Integer] -> Bool
allSignedRef = M.hsAllSignedRef

-- | Extracted contest re-post guard (§6 @if S̄.s > s_c@): our confirmed snapshot number vs the
-- closed/contested one.
contestEligibleRef :: Integer -> Integer -> Bool
contestEligibleRef = M.hsContestEligibleRef

-- | Extracted round-robin leader (the §6 @leader(s)@). Arguments: @m@ where the head has @suc m@ parties,
-- the snapshot number, and a 0-based party index. Bound against the real 'Hydra.HeadLogic.isLeader' in
-- the hydra-node off-chain differential.
leaderRef :: Integer -> Integer -> Integer -> Bool
leaderRef = M.hsLeaderRef
