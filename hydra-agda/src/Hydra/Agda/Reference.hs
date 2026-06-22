-- | Clean Haskell surface over the MAlonzo-extracted decidable reference checkers
-- (@spec\/src\/Hydra\/Protocol\/Reference.agda@). The mangled MAlonzo names below are
-- pinned to the committed @generated/@ tree; if they change, regeneration updates both this
-- shim and @generated/@ together (see @regenerate.sh@) and a stale name is a loud compile error.
module Hydra.Agda.Reference (
  -- * close
  HsCloseTag (..),
  HsOpen (..),
  HsClosed (..),
  Ops,
  mkOps,
  checkClose,

  -- * increment / decrement
  HsIncIO (..),
  OpsInc,
  mkOpsInc,
  checkInc,
  checkDec,

  -- * contest
  HsContestIO (..),
  OpsContest,
  mkOpsContest,
  checkContest,

  -- * fanout / finalPartialFanout
  HsFanout (..),
  OpsFanout,
  mkOpsFanout,
  checkFanout,

  -- * deposit recover (νDeposit)
  HsRecoverIO (..),
  OpsRecover,
  mkOpsRecover,
  checkRecover,

  -- * init (μHead minting policy: token count)
  HsMintIO (..),
  OpsInit,
  mkOpsInit,
  checkInit,

  -- * deposit claim (νDeposit)
  HsClaimIO (..),
  OpsClaim,
  mkOpsClaim,
  checkClaim,

  -- * participant signature (shared: close / contest / increment / decrement)
  HsSignerIO (..),
  checkParticipantSigned,
) where

import MAlonzo.Code.Hydra.Protocol.Reference (
  HsClaimIO (..),
  HsCloseTag (..),
  HsClosed (..),
  HsContestIO (..),
  HsFanout (..),
  HsIncIO (..),
  HsMintIO (..),
  HsOpen (..),
  HsRecoverIO (..),
  HsSignerIO (..),
 )
import MAlonzo.Code.Hydra.Protocol.Reference qualified as M

-- | The injected (mock) crypto\/value boundary — the Agda @Ops@ record. The function decides
-- the conjuncts the decidable layer does not model; the differential tests supply @const True@.
type Ops = M.T_Ops_52

mkOps :: (HsOpen -> HsClosed -> HsCloseTag -> Bool) -> Ops
mkOps = M.C_Ops'46'constructor_125

-- | Extracted decidable close-validity checker. Mirrors the decidable conjuncts of @closeValid@
-- (version\/cp preserved, contesters empty, initial\/any snapshot rules, and the recorded
-- contestation deadline @tfinal == validityHi + cp@, and the bounded validity range @hi - lo \<= cp@);
-- proved to reflect them in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The two trailing
-- 'Integer' arguments are the tx upper and lower validity bounds (POSIXTime ms). The crypto\/value\/
-- accumulator conjuncts remain delegated to the injected (mock) @Ops@.
checkClose :: Ops -> HsOpen -> HsClosed -> HsCloseTag -> Integer -> Integer -> Bool
checkClose = M.d_closeRef'7495'_98

-- | Injected boundary for increment\/decrement.
type OpsInc = M.T_OpsInc_156

mkOpsInc :: (HsIncIO -> Bool) -> OpsInc
mkOpsInc = M.C_OpsInc'46'constructor_2927

-- | Decidable increment checker: the produced version is @suc@ the input version
-- (@VersionNotIncremented@) AND the head value grows by the deposit on BOTH the lovelace component
-- (@adaIn + adaDelta == adaOut@) and the total non-ada token quantity
-- (@nonAdaIn + nonAdaDelta == nonAdaOut@) — the validator's @mustPreserveValue@, now catching a
-- native-token siphon that an ada-only check would miss. Crypto delegated to @OpsInc@.
checkInc :: OpsInc -> HsIncIO -> Bool
checkInc = M.d_incRef'7495'_162

-- | Decidable decrement checker: the produced version is @suc@ the input version AND the head value
-- SHRINKS by the decommit on BOTH the lovelace component (@adaOut + adaDelta == adaIn@) and the total
-- non-ada token quantity (@nonAdaOut + nonAdaDelta == nonAdaIn@): head output + decommitted outputs ==
-- head input, the validator's @mustDecreaseValue@. Crypto delegated to @OpsInc@.
checkDec :: OpsInc -> HsIncIO -> Bool
checkDec = M.d_decRef'7495'_168

-- | Injected boundary for contest.
type OpsContest = M.T_OpsContest_222

mkOpsContest :: (HsContestIO -> Bool) -> OpsContest
mkOpsContest = M.C_OpsContest'46'constructor_3599

-- | Decidable contest checker: version preserved, snapshot strictly increases
-- (@TooOldSnapshot@), exactly one contester appended, AND posted before the contestation deadline
-- (@validityHi \<= tfinal@, the validator's @mustBeWithinContestationPeriod@); proved to reflect
-- @contestValid@ in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The trailing two 'Integer'
-- fields of 'HsContestIO' are the input contestation deadline and the contest tx's upper validity
-- bound (POSIXTime ms), and the conditional deadline-UPDATE rule (@tfinal' == if all-contested then
-- tfinal else tfinal+cp@, the last three 'HsContestIO' fields: tfinal', n, cp). Crypto\/value delegated.
checkContest :: OpsContest -> HsContestIO -> Bool
checkContest = M.d_contestRef'7495'_228

-- | Injected boundary for fanout\/finalPartialFanout.
type OpsFanout = M.T_OpsFanout_258

mkOpsFanout :: (HsFanout -> Bool) -> OpsFanout
mkOpsFanout = M.C_OpsFanout'46'constructor_3853

-- | Decidable fanout checker: @0 < m@ outputs (the validator's @FanoutZeroOutputs@, the §5.8
-- m>0 guard). Accumulator\/value\/burn\/deadline delegated to @OpsFanout@.
checkFanout :: OpsFanout -> HsFanout -> Bool
checkFanout = M.d_fanoutRef'7495'_264

-- | Injected boundary for deposit recover (the recovered-outputs serialisation-hash equality).
type OpsRecover = M.T_OpsRecover_282

mkOpsRecover :: (HsRecoverIO -> Bool) -> OpsRecover
mkOpsRecover = M.C_OpsRecover'46'constructor_3957

-- | Decidable deposit-recover checker (@deposit.ak@ Recover arm): the recover tx is posted strictly
-- after the recover deadline (@tRecover \< validityLo@, i.e. txValidityMin > t_recover, the
-- validator's @DepositPeriodNotReached@); proved to reflect @recoverValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The recovered-outputs hash equality is
-- delegated to the injected (mock) @OpsRecover@.
checkRecover :: OpsRecover -> HsRecoverIO -> Bool
checkRecover = M.d_recoverRef'7495'_288

-- | Injected boundary for init: the seed-spent, ST\/PT placement, and datum-binding μHead checks
-- (they need multi-asset token-name lookup, the value-map API the spec abstracts over).
type OpsInit = M.T_OpsInit_306

mkOpsInit :: (HsMintIO -> Bool) -> OpsInit
mkOpsInit = M.C_OpsInit'46'constructor_4037

-- | Decidable init token-count checker (μHead @validateTokensMinting@'s @checkNumberOfTokens@): the
-- transaction mints exactly @n + 1@ tokens of the head policy (one ST + one PT per party); proved to
-- reflect @initValid@'s mint-count conjunct in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The
-- token placement (ST\/PT into the head output) and datum binding are delegated to @OpsInit@.
checkInit :: OpsInit -> HsMintIO -> Bool
checkInit = M.d_initRef'7495'_312

-- | Injected boundary for deposit claim: the Increment-redeemer coupling of @expect_increment_redeemer@
-- (the deposit is spent by an @Increment@ of the head). The head-id equality half is now checked.
type OpsClaim = M.T_OpsClaim_338

mkOpsClaim :: (HsClaimIO -> Bool) -> OpsClaim
mkOpsClaim = M.C_OpsClaim'46'constructor_4167

-- | Decidable deposit-claim checker (@deposit.ak@ Claim arm): the increment tx collecting the deposit
-- is posted BEFORE the recover deadline (@validityHi \<= tRecover@, i.e. txValidityMax <= t_recover,
-- the validator's @before_deadline@ / @DepositPeriodSurpassed@) AND the deposit datum's head id equals
-- the spent head's id (@depositCid == headCid@, the head-id half of @expect_increment_redeemer@; the
-- ids are passed as the two trailing 'HsClaimIO' integer encodings); proved to reflect @claimValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The Increment-redeemer coupling is delegated.
checkClaim :: OpsClaim -> HsClaimIO -> Bool
checkClaim = M.d_claimRef'7495'_344

-- | Decidable, fully-extractable participant-signature checker (the §5.4–5.7
-- @mustBeSignedByParticipant@, shared by close\/contest\/increment\/decrement): SOME transaction
-- signer holds a participation token in the head value. 'HsSignerIO' carries two @[Integer]@ lists —
-- the tx signers' key-hashes (txInfoSignatories) and the head value's PT token-names (a PT's name IS a
-- key-hash) — both with the same hash→Integer encoding; the check is that they OVERLAP. No injected
-- boundary: a non-participant signer (the validator's @SignerIsNotAParticipant@) makes the lists
-- disjoint and the reference reject. Proved to reflect @signedByParticipant@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ (a postulated extraction-faithfulness boundary).
checkParticipantSigned :: HsSignerIO -> Bool
checkParticipantSigned = M.d_participantSignedRef'7495'_378
