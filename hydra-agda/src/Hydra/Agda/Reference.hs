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
 )
import MAlonzo.Code.Hydra.Protocol.Reference qualified as M

-- | The injected (mock) crypto\/value boundary — the Agda @Ops@ record. The function decides
-- the conjuncts the decidable layer does not model; the differential tests supply @const True@.
type Ops = M.T_Ops_52

mkOps :: (HsOpen -> HsClosed -> HsCloseTag -> Bool) -> Ops
mkOps = M.C_Ops'46'constructor_125

-- | Extracted decidable close-validity checker. Mirrors the decidable conjuncts of @closeValid@
-- (version\/cp preserved, contesters empty, initial\/any snapshot rules, and the recorded
-- contestation deadline @tfinal == validityHi + cp@); proved to reflect them in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The final 'Integer' argument is the tx upper
-- validity bound (POSIXTime ms), against which the deadline is checked. The crypto\/value\/
-- accumulator and bounded-validity conjuncts remain delegated to the injected (mock) @Ops@.
checkClose :: Ops -> HsOpen -> HsClosed -> HsCloseTag -> Integer -> Bool
checkClose = M.d_closeRef'7495'_86

-- | Injected boundary for increment\/decrement.
type OpsInc = M.T_OpsInc_130

mkOpsInc :: (HsIncIO -> Bool) -> OpsInc
mkOpsInc = M.C_OpsInc'46'constructor_2195

-- | Decidable increment checker: the produced version is @suc@ the input version
-- (@VersionNotIncremented@) AND the head value grows by the deposit on the lovelace component
-- (@adaIn + adaDelta == adaOut@, the validator's @mustPreserveValue@). Crypto delegated to @OpsInc@.
checkInc :: OpsInc -> HsIncIO -> Bool
checkInc = M.d_incRef'7495'_136

-- | Decidable decrement checker: the produced version is @suc@ the input version AND the head value
-- SHRINKS by the decommit on the lovelace component (@adaOut + adaDelta == adaIn@, the validator's
-- @mustDecreaseValue@: head output + decommitted outputs == head input). Crypto delegated to @OpsInc@.
checkDec :: OpsInc -> HsIncIO -> Bool
checkDec = M.d_decRef'7495'_142

-- | Injected boundary for contest.
type OpsContest = M.T_OpsContest_184

mkOpsContest :: (HsContestIO -> Bool) -> OpsContest
mkOpsContest = M.C_OpsContest'46'constructor_2603

-- | Decidable contest checker: version preserved, snapshot strictly increases
-- (@TooOldSnapshot@), exactly one contester appended, AND posted before the contestation deadline
-- (@validityHi \<= tfinal@, the validator's @mustBeWithinContestationPeriod@); proved to reflect
-- @contestValid@ in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The trailing two 'Integer'
-- fields of 'HsContestIO' are the input contestation deadline and the contest tx's upper validity
-- bound (POSIXTime ms). Crypto\/value and the conditional deadline-UPDATE rule remain delegated.
checkContest :: OpsContest -> HsContestIO -> Bool
checkContest = M.d_contestRef'7495'_190

-- | Injected boundary for fanout\/finalPartialFanout.
type OpsFanout = M.T_OpsFanout_220

mkOpsFanout :: (HsFanout -> Bool) -> OpsFanout
mkOpsFanout = M.C_OpsFanout'46'constructor_2805

-- | Decidable fanout checker: @0 < m@ outputs (the validator's @FanoutZeroOutputs@, the §5.8
-- m>0 guard). Accumulator\/value\/burn\/deadline delegated to @OpsFanout@.
checkFanout :: OpsFanout -> HsFanout -> Bool
checkFanout = M.d_fanoutRef'7495'_226

-- | Injected boundary for deposit recover (the recovered-outputs serialisation-hash equality).
type OpsRecover = M.T_OpsRecover_244

mkOpsRecover :: (HsRecoverIO -> Bool) -> OpsRecover
mkOpsRecover = M.C_OpsRecover'46'constructor_2909

-- | Decidable deposit-recover checker (@deposit.ak@ Recover arm): the recover tx is posted strictly
-- after the recover deadline (@tRecover \< validityLo@, i.e. txValidityMin > t_recover, the
-- validator's @DepositPeriodNotReached@); proved to reflect @recoverValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The recovered-outputs hash equality is
-- delegated to the injected (mock) @OpsRecover@.
checkRecover :: OpsRecover -> HsRecoverIO -> Bool
checkRecover = M.d_recoverRef'7495'_250

-- | Injected boundary for init: the seed-spent, ST\/PT placement, and datum-binding μHead checks
-- (they need multi-asset token-name lookup, the value-map API the spec abstracts over).
type OpsInit = M.T_OpsInit_268

mkOpsInit :: (HsMintIO -> Bool) -> OpsInit
mkOpsInit = M.C_OpsInit'46'constructor_2989

-- | Decidable init token-count checker (μHead @validateTokensMinting@'s @checkNumberOfTokens@): the
-- transaction mints exactly @n + 1@ tokens of the head policy (one ST + one PT per party); proved to
-- reflect @initValid@'s mint-count conjunct in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The
-- token placement (ST\/PT into the head output) and datum binding are delegated to @OpsInit@.
checkInit :: OpsInit -> HsMintIO -> Bool
checkInit = M.d_initRef'7495'_274

-- | Injected boundary for deposit claim: the own-head binding (the deposit datum's @cid@ equals the
-- head being spent in the same increment, @deposit.ak@'s @expect_increment_redeemer@).
type OpsClaim = M.T_OpsClaim_292

mkOpsClaim :: (HsClaimIO -> Bool) -> OpsClaim
mkOpsClaim = M.C_OpsClaim'46'constructor_3071

-- | Decidable deposit-claim checker (@deposit.ak@ Claim arm): the increment tx collecting the deposit
-- is posted BEFORE the recover deadline (@validityHi \<= tRecover@, i.e. txValidityMax <= t_recover,
-- the validator's @before_deadline@ / @DepositPeriodSurpassed@); proved to reflect @claimValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The own-head binding is delegated to @OpsClaim@.
checkClaim :: OpsClaim -> HsClaimIO -> Bool
checkClaim = M.d_claimRef'7495'_298
