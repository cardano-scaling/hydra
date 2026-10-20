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
  reqSnDecommitSettledRef,
  reqSnDepositDisjointRef,

  -- * reqDec / ackSn / contest guards
  HsPendingCommit (..),
  reqDecEligibleRef,
  notAlreadySignedRef,
  allSignedRef,
  contestEligibleRef,

  -- * round-robin leader (bound to the real 'Hydra.HeadLogic.isLeader')
  leaderRef,
) where

import MAlonzo.Code.Hydra.Protocol.OffChainReference (HsDepositStatus (..), HsPendingCommit (..))
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

-- | Whether the requested version and number can be signed. Arguments are
-- the requested version, local version, confirmed snapshot version, whether
-- the incremental action matches, requested number, seen number and leader check.
-- A request one version behind must preserve the confirmed snapshot's action.
signEligibleRef :: Integer -> Integer -> Integer -> Bool -> Integer -> Integer -> Bool -> Bool
signEligibleRef = M.hsSignEligibleRef

-- | Extracted reqDec eligibility (§6 @wait U_α = ∅ ∧ tx_ω = ⊥@): given the state of the head's
-- recorded pending commit and whether a decommit is in flight, decides whether a new decommit may
-- start.
--
-- The commit argument is 'HsPendingCommit' rather than a @Bool@ on purpose. Which commits hold a
-- decommit back is a protocol question, so the reference answers it: only a registered, unexpired
-- deposit does. A @Bool@ would have moved that decision into whatever projects the node state, and
-- the node clears its @currentDepositTxId@ on neither expiry nor recovery, so the obvious projection
-- ("is the field set") blocks decommits on commits that can never settle.
reqDecEligibleRef :: HsPendingCommit -> Bool -> Bool
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

-- | Extracted reqSn same-version decommit-settlement guard (the node's
-- @requireApplicableDecommitTx@ / @ReqSnDecommitNotSettled@): given whether the request carries a
-- decommit and whether its outputs match the confirmed snapshot's pending decommit, decides whether
-- the request carries the decommit being settled. A settling decommit is never dropped, so a request
-- without one is rejected like one with another decommit.
reqSnDecommitSettledRef :: Bool -> Bool -> Bool
reqSnDecommitSettledRef = M.hsReqSnDecommitSettledRef

-- | Extracted reqSn deposit-disjointness guard (the node's @absorbable@ /
-- @ReqSnDepositOutputsHeld@): given the Integer-encoded TxIns of the active UTxO set, all outputs
-- created by the requested transactions, and the deposit, decides whether the deposit may be
-- included. Both intersections must be empty, even for outputs the request subsequently spends.
reqSnDepositDisjointRef :: [Integer] -> [Integer] -> [Integer] -> Bool
reqSnDepositDisjointRef = M.hsReqSnDepositDisjointRef

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
