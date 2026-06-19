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
) where

import MAlonzo.Code.Hydra.Protocol.Reference (
  HsCloseTag (..),
  HsClosed (..),
  HsContestIO (..),
  HsFanout (..),
  HsIncIO (..),
  HsOpen (..),
 )
import MAlonzo.Code.Hydra.Protocol.Reference qualified as M

-- | The injected (mock) crypto\/value boundary — the Agda @Ops@ record. The function decides
-- the conjuncts the decidable layer does not model; the differential tests supply @const True@.
type Ops = M.T_Ops_48

mkOps :: (HsOpen -> HsClosed -> HsCloseTag -> Bool) -> Ops
mkOps = M.C_Ops'46'constructor_89

-- | Extracted decidable close-validity checker. Mirrors the decidable conjuncts of @closeValid@
-- (version\/cp preserved, contesters empty, initial\/any snapshot rules); proved to reflect them
-- in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The crypto\/value\/accumulator AND the
-- deadline\/bounded-validity conjuncts are delegated to the injected (mock) @Ops@.
checkClose :: Ops -> HsOpen -> HsClosed -> HsCloseTag -> Bool
checkClose = M.d_closeRef'7495'_80

-- | Injected boundary for increment\/decrement.
type OpsInc = M.T_OpsInc_122

mkOpsInc :: (HsIncIO -> Bool) -> OpsInc
mkOpsInc = M.C_OpsInc'46'constructor_2081

-- | Decidable increment checker: the produced version is @suc@ the input version
-- (@VersionNotIncremented@) AND the head value grows by the deposit on the lovelace component
-- (@adaIn + adaDelta == adaOut@, the validator's @mustPreserveValue@). Crypto delegated to @OpsInc@.
checkInc :: OpsInc -> HsIncIO -> Bool
checkInc = M.d_incRef'7495'_128

-- | Decidable decrement checker: version discipline only (its decommit value is not yet supplied as
-- an extractable lovelace); the @HsIncIO@ ada fields are ignored. Crypto\/value delegated.
checkDec :: OpsInc -> HsIncIO -> Bool
checkDec = M.d_decRef'7495'_134

-- | Injected boundary for contest.
type OpsContest = M.T_OpsContest_168

mkOpsContest :: (HsContestIO -> Bool) -> OpsContest
mkOpsContest = M.C_OpsContest'46'constructor_2359

-- | Decidable contest checker: version preserved, snapshot strictly increases
-- (@TooOldSnapshot@), exactly one contester appended. Crypto\/value\/deadline delegated.
checkContest :: OpsContest -> HsContestIO -> Bool
checkContest = M.d_contestRef'7495'_174

-- | Injected boundary for fanout\/finalPartialFanout.
type OpsFanout = M.T_OpsFanout_188

mkOpsFanout :: (HsFanout -> Bool) -> OpsFanout
mkOpsFanout = M.C_OpsFanout'46'constructor_2453

-- | Decidable fanout checker: @0 < m@ outputs (the validator's @FanoutZeroOutputs@, the §5.8
-- m>0 guard). Accumulator\/value\/burn\/deadline delegated to @OpsFanout@.
checkFanout :: OpsFanout -> HsFanout -> Bool
checkFanout = M.d_fanoutRef'7495'_194
