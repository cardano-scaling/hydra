{-# OPTIONS_GHC -w #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Hydra.Protocol.OffChainReference where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.Equality
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Maybe
import qualified MAlonzo.Code.Agda.Builtin.Nat

data HsDepositStatus = InactiveS | ActiveS | ExpiredS deriving (Eq, Show)
data HsPendingCommit = NoCommitP | CommitPendingP | CommitExpiredP | CommitGoneP
   deriving (Eq, Show)
data HsReqTxTerminal = ReqTxFinished | ReqTxWaiting deriving (Eq, Show)
data HsReqTxChange = ReqTxReceived | ReqTxApplied | ReqTxInvalid | ReqTxSnapshotDecided
   deriving (Eq, Show)
data HsReqTxEffect = ReqTxRequestSnapshot deriving (Eq, Show)
data HsReqTxOutcome = HsReqTxOutcome
   { reqTxTerminal :: HsReqTxTerminal
   , reqTxChanges :: [HsReqTxChange]
   , reqTxEffects :: [HsReqTxEffect]
   } deriving (Eq, Show)
data HsSnapshotPhase = NeverSeen | RequestPending | CollectingSignatures | LastConfirmed
   deriving (Eq, Show, Enum, Bounded)
data HsSnapshotChange = RecordSignature | ConfirmSnapshot | DecideNextSnapshot | RecordSnapshot
   deriving (Eq, Show)
data HsSnapshotEffect = SendAcknowledgement | SendSnapshotRequest deriving (Eq, Show)
data HsSnapshotWait = PreviousUnconfirmed | VersionMismatch | MissingTransactions | SnapshotNotSeen
   deriving (Eq, Show)
data HsSnapshotRejection = InvalidNumber | NotLeader | TransactionDoesNotApply | AlreadySigned | InvalidSignatureSet
   deriving (Eq, Show)
data HsSnapshotOutcome
   = SnapshotCompleted [HsSnapshotChange] [HsSnapshotEffect]
   | SnapshotWaiting HsSnapshotWait
   | SnapshotRejected HsSnapshotRejection
   deriving (Eq, Show)
data HsLifecyclePhase = IdleHead | OpenHead | ClosedHead | FanoutProgress | FinalizedHead deriving (Eq, Show, Enum, Bounded)
data HsLifecycleEvent = Opened | Closed | Contested | Ready | FanoutStarted | PartialObserved | FanoutReverted | FannedOut | Unchanged deriving (Eq, Show, Enum, Bounded)
data HsCoordinationPhase = OutsideHead | NoRequestSeen | SnapshotIdle | RequestSent | Collecting deriving (Eq, Show, Enum, Bounded)
data HsCoordinationEvent = HeadOpened | HeadClosed | RequestDecided | SnapshotRequested | SnapshotConfirmed | LocalCleared | SettlementObserved | OtherEvent deriving (Eq, Show, Enum, Bounded)
data HsTrackedPhase = MissingDeposit | InactiveDeposit | ActiveDeposit | ExpiredDeposit | ConsumedDeposit deriving (Eq, Show, Enum, Bounded)
data HsReqDecOutcome = ReqDecWaitCommit | ReqDecInvalidCommit
   | ReqDecWaitExisting | ReqDecInvalidExisting | ReqDecWaitLedger
   | ReqDecInvalidLedger | ReqDecEmptyOutputs | ReqDecRecorded
   | ReqDecRequested deriving (Eq, Show)
data HsRecoverOutcome = RecoveryMissing | RecoveryBlocked | RecoveryPosted deriving (Eq, Show)
data HsCommitPhase = NoCommit | CommitSelected | AwaitingCommitSettlement deriving (Eq, Show)
-- Hydra.Protocol.OffChainReference.DepositStatusᶜ
d_DepositStatus'7580'_6 = ()
type T_DepositStatus'7580'_6 = HsDepositStatus
pattern C_inactive'7580'_8 = InactiveS
pattern C_active'7580'_10 = ActiveS
pattern C_expired'7580'_12 = ExpiredS
check_inactive'7580'_8 :: T_DepositStatus'7580'_6
check_inactive'7580'_8 = InactiveS
check_active'7580'_10 :: T_DepositStatus'7580'_6
check_active'7580'_10 = ActiveS
check_expired'7580'_12 :: T_DepositStatus'7580'_6
check_expired'7580'_12 = ExpiredS
cover_DepositStatus'7580'_6 :: HsDepositStatus -> ()
cover_DepositStatus'7580'_6 x
  = case x of
      InactiveS -> ()
      ActiveS -> ()
      ExpiredS -> ()
-- Hydra.Protocol.OffChainReference.if_then_else_
d_if_then_else__16 :: () -> Bool -> AgdaAny -> AgdaAny -> AgdaAny
d_if_then_else__16 ~v0 v1 v2 v3 = du_if_then_else__16 v1 v2 v3
du_if_then_else__16 :: Bool -> AgdaAny -> AgdaAny -> AgdaAny
du_if_then_else__16 v0 v1 v2 = if coe v0 then coe v1 else coe v2
-- Hydra.Protocol.OffChainReference.depositStatusRef
hsDepositStatusRef ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_DepositStatus'7580'_6
hsDepositStatusRef = coe d_depositStatusRef_22
d_depositStatusRef_22 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_DepositStatus'7580'_6
d_depositStatusRef_22 v0 v1 v2 v3 v4
  = coe
      du_if_then_else__16
      (coe
         ltInt (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1 v2)
         (coe v4))
      (coe C_expired'7580'_12)
      (coe
         du_if_then_else__16
         (coe ltInt (coe addInt (coe v0) (coe v3)) (coe v4))
         (coe C_active'7580'_10) (coe C_inactive'7580'_8))
-- Hydra.Protocol.OffChainReference._&&_
d__'38''38'__34 :: Bool -> Bool -> Bool
d__'38''38'__34 v0 v1 = if coe v0 then coe v1 else coe v0
-- Hydra.Protocol.OffChainReference.signEligibleRef
hsSignEligibleRef ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool
hsSignEligibleRef = coe d_signEligibleRef_38
d_signEligibleRef_38 ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool
d_signEligibleRef_38 v0 v1 v2 v3 v4
  = coe
      d__'38''38'__34 (coe eqInt (coe v0) (coe v1))
      (coe
         d__'38''38'__34
         (coe eqInt (coe v2) (coe addInt (coe (1 :: Integer)) (coe v3)))
         (coe v4))
-- Hydra.Protocol.OffChainReference._||_
d__'124''124'__50 :: Bool -> Bool -> Bool
d__'124''124'__50 v0 v1 = if coe v0 then coe v0 else coe v1
-- Hydra.Protocol.OffChainReference.not
d_not_54 :: Bool -> Bool
d_not_54 v0
  = if coe v0
      then coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      else coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
-- Hydra.Protocol.OffChainReference.elemᵇ
d_elem'7495'_56 :: Integer -> [Integer] -> Bool
d_elem'7495'_56 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__50 (coe eqInt (coe v0) (coe v2))
             (coe d_elem'7495'_56 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.PendingCommitᶜ
d_PendingCommit'7580'_64 = ()
type T_PendingCommit'7580'_64 = HsPendingCommit
pattern C_noCommit'7580'_66 = NoCommitP
pattern C_commitPending'7580'_68 = CommitPendingP
pattern C_commitExpired'7580'_70 = CommitExpiredP
pattern C_commitGone'7580'_72 = CommitGoneP
check_noCommit'7580'_66 :: T_PendingCommit'7580'_64
check_noCommit'7580'_66 = NoCommitP
check_commitPending'7580'_68 :: T_PendingCommit'7580'_64
check_commitPending'7580'_68 = CommitPendingP
check_commitExpired'7580'_70 :: T_PendingCommit'7580'_64
check_commitExpired'7580'_70 = CommitExpiredP
check_commitGone'7580'_72 :: T_PendingCommit'7580'_64
check_commitGone'7580'_72 = CommitGoneP
cover_PendingCommit'7580'_64 :: HsPendingCommit -> ()
cover_PendingCommit'7580'_64 x
  = case x of
      NoCommitP -> ()
      CommitPendingP -> ()
      CommitExpiredP -> ()
      CommitGoneP -> ()
-- Hydra.Protocol.OffChainReference.commitBlocksᵇ
d_commitBlocks'7495'_74 :: T_PendingCommit'7580'_64 -> Bool
d_commitBlocks'7495'_74 v0
  = case coe v0 of
      C_noCommit'7580'_66 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      C_commitPending'7580'_68
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      C_commitExpired'7580'_70
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      C_commitGone'7580'_72
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.reqDecEligibleRef
hsReqDecEligibleRef :: T_PendingCommit'7580'_64 -> Bool -> Bool
hsReqDecEligibleRef = coe d_reqDecEligibleRef_76
d_reqDecEligibleRef_76 :: T_PendingCommit'7580'_64 -> Bool -> Bool
d_reqDecEligibleRef_76 v0 v1
  = coe
      d__'38''38'__34
      (coe d_not_54 (coe d_commitBlocks'7495'_74 (coe v0)))
      (coe d_not_54 (coe v1))
-- Hydra.Protocol.OffChainReference.reqSnNotBothRef
hsReqSnNotBothRef :: Bool -> Bool -> Bool
hsReqSnNotBothRef = coe d_reqSnNotBothRef_82
d_reqSnNotBothRef_82 :: Bool -> Bool -> Bool
d_reqSnNotBothRef_82 v0 v1
  = coe d_not_54 (coe d__'38''38'__34 (coe v0) (coe v1))
-- Hydra.Protocol.OffChainReference.reqSnDecommitOutputsRef
hsReqSnDecommitOutputsRef :: Integer -> Bool
hsReqSnDecommitOutputsRef = coe d_reqSnDecommitOutputsRef_88
d_reqSnDecommitOutputsRef_88 :: Integer -> Bool
d_reqSnDecommitOutputsRef_88 v0
  = coe ltInt (coe (0 :: Integer)) (coe v0)
-- Hydra.Protocol.OffChainReference.reqSnDepositSettledRef
hsReqSnDepositSettledRef :: Bool -> Integer -> Integer -> Bool
hsReqSnDepositSettledRef = coe d_reqSnDepositSettledRef_92
d_reqSnDepositSettledRef_92 :: Bool -> Integer -> Integer -> Bool
d_reqSnDepositSettledRef_92 v0 v1 v2
  = coe d__'38''38'__34 (coe v0) (coe eqInt (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.reqSnDecommitSettledRef
hsReqSnDecommitSettledRef :: Bool -> Bool -> Bool
hsReqSnDecommitSettledRef = coe d_reqSnDecommitSettledRef_100
d_reqSnDecommitSettledRef_100 :: Bool -> Bool -> Bool
d_reqSnDecommitSettledRef_100 v0 v1
  = coe d__'38''38'__34 (coe v0) (coe v1)
-- Hydra.Protocol.OffChainReference.disjointᵇ
d_disjoint'7495'_106 :: [Integer] -> [Integer] -> Bool
d_disjoint'7495'_106 v0 v1
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v2 v3
        -> coe
             d__'38''38'__34
             (coe d_not_54 (coe d_elem'7495'_56 (coe v2) (coe v1)))
             (coe d_disjoint'7495'_106 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.reqSnDepositDisjointRef
hsReqSnDepositDisjointRef ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsReqSnDepositDisjointRef = coe d_reqSnDepositDisjointRef_114
d_reqSnDepositDisjointRef_114 ::
  [Integer] -> [Integer] -> [Integer] -> Bool
d_reqSnDepositDisjointRef_114 v0 v1 v2
  = coe
      d__'38''38'__34 (coe d_disjoint'7495'_106 (coe v2) (coe v0))
      (coe d_disjoint'7495'_106 (coe v2) (coe v1))
-- Hydra.Protocol.OffChainReference.notAlreadySignedRef
hsNotAlreadySignedRef ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  Integer -> Bool
hsNotAlreadySignedRef = coe d_notAlreadySignedRef_122
d_notAlreadySignedRef_122 :: [Integer] -> Integer -> Bool
d_notAlreadySignedRef_122 v0 v1
  = coe d_not_54 (coe d_elem'7495'_56 (coe v1) (coe v0))
-- Hydra.Protocol.OffChainReference.allBelowᵇ
d_allBelow'7495'_128 :: Integer -> [Integer] -> Bool
d_allBelow'7495'_128 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (coe
                d__'38''38'__34 (coe d_elem'7495'_56 (coe v2) (coe v1))
                (coe d_allBelow'7495'_128 (coe v2) (coe v1)))
-- Hydra.Protocol.OffChainReference.allSignedRef
hsAllSignedRef ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsAllSignedRef = coe d_allSignedRef_134
d_allSignedRef_134 :: Integer -> [Integer] -> Bool
d_allSignedRef_134 v0 v1
  = coe d_allBelow'7495'_128 (coe v0) (coe v1)
-- Hydra.Protocol.OffChainReference.contestEligibleRef
hsContestEligibleRef :: Integer -> Integer -> Bool
hsContestEligibleRef = coe d_contestEligibleRef_140
d_contestEligibleRef_140 :: Integer -> Integer -> Bool
d_contestEligibleRef_140 v0 v1 = coe ltInt (coe v1) (coe v0)
-- Hydra.Protocol.OffChainReference.modSuc
d_modSuc_146 :: Integer -> Integer -> Integer
d_modSuc_146 v0 v1
  = coe remInt (coe v0) (coe addInt (coe (1 :: Integer)) (coe v1))
-- Hydra.Protocol.OffChainReference.leaderRef
hsLeaderRef :: Integer -> Integer -> Integer -> Bool
hsLeaderRef = coe d_leaderRef_152
d_leaderRef_152 :: Integer -> Integer -> Integer -> Bool
d_leaderRef_152 v0 v1 v2
  = coe
      eqInt (coe d_modSuc_146 (coe addInt (coe v0) (coe v1)) (coe v0))
      (coe v2)
-- Hydra.Protocol.OffChainReference.ReqTxTerminalᶜ
d_ReqTxTerminal'7580'_160 = ()
type T_ReqTxTerminal'7580'_160 = HsReqTxTerminal
pattern C_reqTxFinished'7580'_162 = ReqTxFinished
pattern C_reqTxWaiting'7580'_164 = ReqTxWaiting
check_reqTxFinished'7580'_162 :: T_ReqTxTerminal'7580'_160
check_reqTxFinished'7580'_162 = ReqTxFinished
check_reqTxWaiting'7580'_164 :: T_ReqTxTerminal'7580'_160
check_reqTxWaiting'7580'_164 = ReqTxWaiting
cover_ReqTxTerminal'7580'_160 :: HsReqTxTerminal -> ()
cover_ReqTxTerminal'7580'_160 x
  = case x of
      ReqTxFinished -> ()
      ReqTxWaiting -> ()
-- Hydra.Protocol.OffChainReference.ReqTxChangeᶜ
d_ReqTxChange'7580'_166 = ()
type T_ReqTxChange'7580'_166 = HsReqTxChange
pattern C_reqTxReceived'7580'_168 = ReqTxReceived
pattern C_reqTxApplied'7580'_170 = ReqTxApplied
pattern C_reqTxInvalid'7580'_172 = ReqTxInvalid
pattern C_reqTxSnapshotDecided'7580'_174 = ReqTxSnapshotDecided
check_reqTxReceived'7580'_168 :: T_ReqTxChange'7580'_166
check_reqTxReceived'7580'_168 = ReqTxReceived
check_reqTxApplied'7580'_170 :: T_ReqTxChange'7580'_166
check_reqTxApplied'7580'_170 = ReqTxApplied
check_reqTxInvalid'7580'_172 :: T_ReqTxChange'7580'_166
check_reqTxInvalid'7580'_172 = ReqTxInvalid
check_reqTxSnapshotDecided'7580'_174 :: T_ReqTxChange'7580'_166
check_reqTxSnapshotDecided'7580'_174 = ReqTxSnapshotDecided
cover_ReqTxChange'7580'_166 :: HsReqTxChange -> ()
cover_ReqTxChange'7580'_166 x
  = case x of
      ReqTxReceived -> ()
      ReqTxApplied -> ()
      ReqTxInvalid -> ()
      ReqTxSnapshotDecided -> ()
-- Hydra.Protocol.OffChainReference.ReqTxEffectᶜ
d_ReqTxEffect'7580'_176 = ()
type T_ReqTxEffect'7580'_176 = HsReqTxEffect
pattern C_reqTxRequestSnapshot'7580'_178 = ReqTxRequestSnapshot
check_reqTxRequestSnapshot'7580'_178 :: T_ReqTxEffect'7580'_176
check_reqTxRequestSnapshot'7580'_178 = ReqTxRequestSnapshot
cover_ReqTxEffect'7580'_176 :: HsReqTxEffect -> ()
cover_ReqTxEffect'7580'_176 x
  = case x of
      ReqTxRequestSnapshot -> ()
-- Hydra.Protocol.OffChainReference.ReqTxOutcomeᶜ
d_ReqTxOutcome'7580'_180 = ()
type T_ReqTxOutcome'7580'_180 = HsReqTxOutcome
pattern C_reqTxOutcome'7580'_194 a0 a1 a2 = HsReqTxOutcome a0 a1 a2
check_reqTxOutcome'7580'_194 ::
  T_ReqTxTerminal'7580'_160 ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () T_ReqTxChange'7580'_166 ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () T_ReqTxEffect'7580'_176 ->
  T_ReqTxOutcome'7580'_180
check_reqTxOutcome'7580'_194 = HsReqTxOutcome
cover_ReqTxOutcome'7580'_180 :: HsReqTxOutcome -> ()
cover_ReqTxOutcome'7580'_180 x
  = case x of
      HsReqTxOutcome _ _ _ -> ()
-- Hydra.Protocol.OffChainReference.ReqTxOutcomeᶜ.reqTxTerminalᶜ
d_reqTxTerminal'7580'_188 ::
  T_ReqTxOutcome'7580'_180 -> T_ReqTxTerminal'7580'_160
d_reqTxTerminal'7580'_188 v0
  = case coe v0 of
      C_reqTxOutcome'7580'_194 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.ReqTxOutcomeᶜ.reqTxChangesᶜ
d_reqTxChanges'7580'_190 ::
  T_ReqTxOutcome'7580'_180 -> [T_ReqTxChange'7580'_166]
d_reqTxChanges'7580'_190 v0
  = case coe v0 of
      C_reqTxOutcome'7580'_194 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.ReqTxOutcomeᶜ.reqTxEffectsᶜ
d_reqTxEffects'7580'_192 ::
  T_ReqTxOutcome'7580'_180 -> [T_ReqTxEffect'7580'_176]
d_reqTxEffects'7580'_192 v0
  = case coe v0 of
      C_reqTxOutcome'7580'_194 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.reqTxOutcomeRef
hsReqTxOutcomeRef ::
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqTxOutcome'7580'_180
hsReqTxOutcomeRef = coe d_reqTxOutcomeRef_196
d_reqTxOutcomeRef_196 ::
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqTxOutcome'7580'_180
d_reqTxOutcomeRef_196 v0 v1 v2 v3 v4 v5
  = if coe v0
      then coe
             du_if_then_else__16
             (coe
                d__'38''38'__34 (coe d_not_54 (coe v2))
                (coe d_leaderRef_152 (coe v3) (coe v4) (coe v5)))
             (coe
                C_reqTxOutcome'7580'_194 (coe C_reqTxFinished'7580'_162)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                   (coe C_reqTxReceived'7580'_168)
                   (coe
                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                      (coe C_reqTxApplied'7580'_170)
                      (coe
                         MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                         (coe C_reqTxSnapshotDecided'7580'_174)
                         (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                   (coe C_reqTxRequestSnapshot'7580'_178)
                   (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
             (coe
                C_reqTxOutcome'7580'_194 (coe C_reqTxFinished'7580'_162)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                   (coe C_reqTxReceived'7580'_168)
                   (coe
                      MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                      (coe C_reqTxApplied'7580'_170)
                      (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
      else (case coe v1 of
              0 -> coe
                     C_reqTxOutcome'7580'_194 (coe C_reqTxFinished'7580'_162)
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                        (coe C_reqTxReceived'7580'_168)
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe C_reqTxInvalid'7580'_172)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
              _ -> coe
                     C_reqTxOutcome'7580'_194 (coe C_reqTxWaiting'7580'_164)
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                        (coe C_reqTxReceived'7580'_168)
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
-- Hydra.Protocol.OffChainReference.SnapshotPhaseᶜ
d_SnapshotPhase'7580'_206 = ()
type T_SnapshotPhase'7580'_206 = HsSnapshotPhase
pattern C_neverSeen'7580'_208 = NeverSeen
pattern C_requestPending'7580'_210 = RequestPending
pattern C_collectingSignatures'7580'_212 = CollectingSignatures
pattern C_lastConfirmed'7580'_214 = LastConfirmed
check_neverSeen'7580'_208 :: T_SnapshotPhase'7580'_206
check_neverSeen'7580'_208 = NeverSeen
check_requestPending'7580'_210 :: T_SnapshotPhase'7580'_206
check_requestPending'7580'_210 = RequestPending
check_collectingSignatures'7580'_212 :: T_SnapshotPhase'7580'_206
check_collectingSignatures'7580'_212 = CollectingSignatures
check_lastConfirmed'7580'_214 :: T_SnapshotPhase'7580'_206
check_lastConfirmed'7580'_214 = LastConfirmed
cover_SnapshotPhase'7580'_206 :: HsSnapshotPhase -> ()
cover_SnapshotPhase'7580'_206 x
  = case x of
      NeverSeen -> ()
      RequestPending -> ()
      CollectingSignatures -> ()
      LastConfirmed -> ()
-- Hydra.Protocol.OffChainReference.SnapshotChangeᶜ
d_SnapshotChange'7580'_216 = ()
type T_SnapshotChange'7580'_216 = HsSnapshotChange
pattern C_recordSignature'7580'_218 = RecordSignature
pattern C_confirmSnapshot'7580'_220 = ConfirmSnapshot
pattern C_decideNextSnapshot'7580'_222 = DecideNextSnapshot
pattern C_recordSnapshot'7580'_224 = RecordSnapshot
check_recordSignature'7580'_218 :: T_SnapshotChange'7580'_216
check_recordSignature'7580'_218 = RecordSignature
check_confirmSnapshot'7580'_220 :: T_SnapshotChange'7580'_216
check_confirmSnapshot'7580'_220 = ConfirmSnapshot
check_decideNextSnapshot'7580'_222 :: T_SnapshotChange'7580'_216
check_decideNextSnapshot'7580'_222 = DecideNextSnapshot
check_recordSnapshot'7580'_224 :: T_SnapshotChange'7580'_216
check_recordSnapshot'7580'_224 = RecordSnapshot
cover_SnapshotChange'7580'_216 :: HsSnapshotChange -> ()
cover_SnapshotChange'7580'_216 x
  = case x of
      RecordSignature -> ()
      ConfirmSnapshot -> ()
      DecideNextSnapshot -> ()
      RecordSnapshot -> ()
-- Hydra.Protocol.OffChainReference.SnapshotEffectᶜ
d_SnapshotEffect'7580'_226 = ()
type T_SnapshotEffect'7580'_226 = HsSnapshotEffect
pattern C_sendAcknowledgement'7580'_228 = SendAcknowledgement
pattern C_sendSnapshotRequest'7580'_230 = SendSnapshotRequest
check_sendAcknowledgement'7580'_228 :: T_SnapshotEffect'7580'_226
check_sendAcknowledgement'7580'_228 = SendAcknowledgement
check_sendSnapshotRequest'7580'_230 :: T_SnapshotEffect'7580'_226
check_sendSnapshotRequest'7580'_230 = SendSnapshotRequest
cover_SnapshotEffect'7580'_226 :: HsSnapshotEffect -> ()
cover_SnapshotEffect'7580'_226 x
  = case x of
      SendAcknowledgement -> ()
      SendSnapshotRequest -> ()
-- Hydra.Protocol.OffChainReference.SnapshotWaitᶜ
d_SnapshotWait'7580'_232 = ()
type T_SnapshotWait'7580'_232 = HsSnapshotWait
pattern C_previousUnconfirmed'7580'_234 = PreviousUnconfirmed
pattern C_versionMismatch'7580'_236 = VersionMismatch
pattern C_missingTransactions'7580'_238 = MissingTransactions
pattern C_snapshotNotSeen'7580'_240 = SnapshotNotSeen
check_previousUnconfirmed'7580'_234 :: T_SnapshotWait'7580'_232
check_previousUnconfirmed'7580'_234 = PreviousUnconfirmed
check_versionMismatch'7580'_236 :: T_SnapshotWait'7580'_232
check_versionMismatch'7580'_236 = VersionMismatch
check_missingTransactions'7580'_238 :: T_SnapshotWait'7580'_232
check_missingTransactions'7580'_238 = MissingTransactions
check_snapshotNotSeen'7580'_240 :: T_SnapshotWait'7580'_232
check_snapshotNotSeen'7580'_240 = SnapshotNotSeen
cover_SnapshotWait'7580'_232 :: HsSnapshotWait -> ()
cover_SnapshotWait'7580'_232 x
  = case x of
      PreviousUnconfirmed -> ()
      VersionMismatch -> ()
      MissingTransactions -> ()
      SnapshotNotSeen -> ()
-- Hydra.Protocol.OffChainReference.SnapshotRejectionᶜ
d_SnapshotRejection'7580'_242 = ()
type T_SnapshotRejection'7580'_242 = HsSnapshotRejection
pattern C_invalidNumber'7580'_244 = InvalidNumber
pattern C_notLeader'7580'_246 = NotLeader
pattern C_transactionDoesNotApply'7580'_248 = TransactionDoesNotApply
pattern C_alreadySigned'7580'_250 = AlreadySigned
pattern C_invalidSignatureSet'7580'_252 = InvalidSignatureSet
check_invalidNumber'7580'_244 :: T_SnapshotRejection'7580'_242
check_invalidNumber'7580'_244 = InvalidNumber
check_notLeader'7580'_246 :: T_SnapshotRejection'7580'_242
check_notLeader'7580'_246 = NotLeader
check_transactionDoesNotApply'7580'_248 ::
  T_SnapshotRejection'7580'_242
check_transactionDoesNotApply'7580'_248 = TransactionDoesNotApply
check_alreadySigned'7580'_250 :: T_SnapshotRejection'7580'_242
check_alreadySigned'7580'_250 = AlreadySigned
check_invalidSignatureSet'7580'_252 ::
  T_SnapshotRejection'7580'_242
check_invalidSignatureSet'7580'_252 = InvalidSignatureSet
cover_SnapshotRejection'7580'_242 :: HsSnapshotRejection -> ()
cover_SnapshotRejection'7580'_242 x
  = case x of
      InvalidNumber -> ()
      NotLeader -> ()
      TransactionDoesNotApply -> ()
      AlreadySigned -> ()
      InvalidSignatureSet -> ()
-- Hydra.Protocol.OffChainReference.SnapshotOutcomeᶜ
d_SnapshotOutcome'7580'_254 = ()
type T_SnapshotOutcome'7580'_254 = HsSnapshotOutcome
pattern C_snapshotCompleted'7580'_256 a0 a1 = SnapshotCompleted a0 a1
pattern C_snapshotWaiting'7580'_258 a0 = SnapshotWaiting a0
pattern C_snapshotRejected'7580'_260 a0 = SnapshotRejected a0
check_snapshotCompleted'7580'_256 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () T_SnapshotChange'7580'_216 ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () T_SnapshotEffect'7580'_226 ->
  T_SnapshotOutcome'7580'_254
check_snapshotCompleted'7580'_256 = SnapshotCompleted
check_snapshotWaiting'7580'_258 ::
  T_SnapshotWait'7580'_232 -> T_SnapshotOutcome'7580'_254
check_snapshotWaiting'7580'_258 = SnapshotWaiting
check_snapshotRejected'7580'_260 ::
  T_SnapshotRejection'7580'_242 -> T_SnapshotOutcome'7580'_254
check_snapshotRejected'7580'_260 = SnapshotRejected
cover_SnapshotOutcome'7580'_254 :: HsSnapshotOutcome -> ()
cover_SnapshotOutcome'7580'_254 x
  = case x of
      SnapshotCompleted _ _ -> ()
      SnapshotWaiting _ -> ()
      SnapshotRejected _ -> ()
-- Hydra.Protocol.OffChainReference.reqSnOutcomeRef
hsReqSnOutcomeRef ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Bool -> T_SnapshotOutcome'7580'_254
hsReqSnOutcomeRef = coe d_reqSnOutcomeRef_262
d_reqSnOutcomeRef_262 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Bool -> T_SnapshotOutcome'7580'_254
d_reqSnOutcomeRef_262 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      du_if_then_else__16
      (coe
         d_not_54
         (coe eqInt (coe v4) (coe addInt (coe (1 :: Integer)) (coe v0))))
      (coe C_snapshotRejected'7580'_260 (coe C_invalidNumber'7580'_244))
      (coe
         du_if_then_else__16
         (coe d_not_54 (coe d_leaderRef_152 (coe v5) (coe v4) (coe v6)))
         (coe C_snapshotRejected'7580'_260 (coe C_notLeader'7580'_246))
         (coe
            du_if_then_else__16 (coe d_not_54 (coe eqInt (coe v1) (coe v0)))
            (coe
               C_snapshotWaiting'7580'_258 (coe C_previousUnconfirmed'7580'_234))
            (coe
               du_if_then_else__16 (coe d_not_54 (coe eqInt (coe v2) (coe v3)))
               (coe C_snapshotWaiting'7580'_258 (coe C_versionMismatch'7580'_236))
               (coe
                  du_if_then_else__16 (coe ltInt (coe (0 :: Integer)) (coe v7))
                  (coe
                     C_snapshotWaiting'7580'_258 (coe C_missingTransactions'7580'_238))
                  (coe
                     du_if_then_else__16 (coe d_not_54 (coe v8))
                     (coe
                        C_snapshotRejected'7580'_260
                        (coe C_transactionDoesNotApply'7580'_248))
                     (coe
                        C_snapshotCompleted'7580'_256
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe C_recordSnapshot'7580'_224)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe C_sendAcknowledgement'7580'_228)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))))
-- Hydra.Protocol.OffChainReference.isLastConfirmed
d_isLastConfirmed_282 :: T_SnapshotPhase'7580'_206 -> Bool
d_isLastConfirmed_282 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         C_lastConfirmed'7580'_214
           -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
         _ -> coe v1)
-- Hydra.Protocol.OffChainReference.isCollecting
d_isCollecting_284 :: T_SnapshotPhase'7580'_206 -> Bool
d_isCollecting_284 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         C_collectingSignatures'7580'_212
           -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
         _ -> coe v1)
-- Hydra.Protocol.OffChainReference.ackSnOutcomeRef
hsAckSnOutcomeRef ::
  T_SnapshotPhase'7580'_206 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  Integer ->
  Integer -> Bool -> Bool -> Integer -> T_SnapshotOutcome'7580'_254
hsAckSnOutcomeRef = coe d_ackSnOutcomeRef_286
d_ackSnOutcomeRef_286 ::
  T_SnapshotPhase'7580'_206 ->
  Integer ->
  Integer ->
  [Integer] ->
  Integer ->
  Integer -> Bool -> Bool -> Integer -> T_SnapshotOutcome'7580'_254
d_ackSnOutcomeRef_286 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      du_if_then_else__16
      (coe
         d_not_54
         (coe
            d__'124''124'__50 (coe eqInt (coe v2) (coe v1))
            (coe eqInt (coe v2) (coe addInt (coe (1 :: Integer)) (coe v1)))))
      (coe C_snapshotRejected'7580'_260 (coe C_invalidNumber'7580'_244))
      (coe
         du_if_then_else__16
         (coe
            d__'38''38'__34 (coe d_isLastConfirmed_282 (coe v0))
            (coe eqInt (coe v2) (coe v1)))
         (coe
            C_snapshotCompleted'7580'_256
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
         (coe
            du_if_then_else__16
            (coe
               d_not_54
               (coe
                  d__'38''38'__34 (coe d_isCollecting_284 (coe v0))
                  (coe eqInt (coe v2) (coe v1))))
            (coe C_snapshotWaiting'7580'_258 (coe C_snapshotNotSeen'7580'_240))
            (coe
               du_if_then_else__16 (coe d_elem'7495'_56 (coe v4) (coe v3))
               (coe C_snapshotRejected'7580'_260 (coe C_alreadySigned'7580'_250))
               (coe
                  du_if_then_else__16
                  (coe
                     d_not_54
                     (coe
                        d_allSignedRef_134 (coe addInt (coe (1 :: Integer)) (coe v5))
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4) (coe v3))))
                  (coe
                     C_snapshotCompleted'7580'_256
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                        (coe C_recordSignature'7580'_218)
                        (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                  (coe
                     du_if_then_else__16 (coe d_not_54 (coe v6))
                     (coe
                        C_snapshotRejected'7580'_260 (coe C_invalidSignatureSet'7580'_252))
                     (coe
                        du_if_then_else__16
                        (coe
                           d__'38''38'__34 (coe v7)
                           (coe
                              d_leaderRef_152 (coe v5) (coe addInt (coe (1 :: Integer)) (coe v2))
                              (coe v8)))
                        (coe
                           C_snapshotCompleted'7580'_256
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe C_recordSignature'7580'_218)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                 (coe C_confirmSnapshot'7580'_220)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                    (coe C_decideNextSnapshot'7580'_222)
                                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe C_sendSnapshotRequest'7580'_230)
                              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                        (coe
                           C_snapshotCompleted'7580'_256
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe C_recordSignature'7580'_218)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                 (coe C_confirmSnapshot'7580'_220)
                                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))))
-- Hydra.Protocol.OffChainReference.reqTxKnownOutcomeRef
hsReqTxKnownOutcomeRef ::
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqTxOutcome'7580'_180
hsReqTxKnownOutcomeRef = coe d_reqTxKnownOutcomeRef_306
d_reqTxKnownOutcomeRef_306 ::
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqTxOutcome'7580'_180
d_reqTxKnownOutcomeRef_306 v0 v1 v2 v3 v4 v5 v6
  = if coe v0
      then coe
             C_reqTxOutcome'7580'_194 (coe C_reqTxFinished'7580'_162)
             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
      else coe
             d_reqTxOutcomeRef_196 (coe v1) (coe v2) (coe v3) (coe v4) (coe v5)
             (coe v6)
-- Hydra.Protocol.OffChainReference.LifecyclePhaseᶜ
d_LifecyclePhase'7580'_320 = ()
type T_LifecyclePhase'7580'_320 = HsLifecyclePhase
pattern C_idleHead'7580'_322 = IdleHead
pattern C_openHead'7580'_324 = OpenHead
pattern C_closedHead'7580'_326 = ClosedHead
pattern C_fanoutProgress'7580'_328 = FanoutProgress
pattern C_finalizedHead'7580'_330 = FinalizedHead
check_idleHead'7580'_322 :: T_LifecyclePhase'7580'_320
check_idleHead'7580'_322 = IdleHead
check_openHead'7580'_324 :: T_LifecyclePhase'7580'_320
check_openHead'7580'_324 = OpenHead
check_closedHead'7580'_326 :: T_LifecyclePhase'7580'_320
check_closedHead'7580'_326 = ClosedHead
check_fanoutProgress'7580'_328 :: T_LifecyclePhase'7580'_320
check_fanoutProgress'7580'_328 = FanoutProgress
check_finalizedHead'7580'_330 :: T_LifecyclePhase'7580'_320
check_finalizedHead'7580'_330 = FinalizedHead
cover_LifecyclePhase'7580'_320 :: HsLifecyclePhase -> ()
cover_LifecyclePhase'7580'_320 x
  = case x of
      IdleHead -> ()
      OpenHead -> ()
      ClosedHead -> ()
      FanoutProgress -> ()
      FinalizedHead -> ()
-- Hydra.Protocol.OffChainReference.LifecycleEventᶜ
d_LifecycleEvent'7580'_332 = ()
type T_LifecycleEvent'7580'_332 = HsLifecycleEvent
pattern C_opened'7580'_334 = Opened
pattern C_closed'7580'_336 = Closed
pattern C_contested'7580'_338 = Contested
pattern C_ready'7580'_340 = Ready
pattern C_fanoutStarted'7580'_342 = FanoutStarted
pattern C_partialObserved'7580'_344 = PartialObserved
pattern C_fanoutReverted'7580'_346 = FanoutReverted
pattern C_fannedOut'7580'_348 = FannedOut
pattern C_unchanged'7580'_350 = Unchanged
check_opened'7580'_334 :: T_LifecycleEvent'7580'_332
check_opened'7580'_334 = Opened
check_closed'7580'_336 :: T_LifecycleEvent'7580'_332
check_closed'7580'_336 = Closed
check_contested'7580'_338 :: T_LifecycleEvent'7580'_332
check_contested'7580'_338 = Contested
check_ready'7580'_340 :: T_LifecycleEvent'7580'_332
check_ready'7580'_340 = Ready
check_fanoutStarted'7580'_342 :: T_LifecycleEvent'7580'_332
check_fanoutStarted'7580'_342 = FanoutStarted
check_partialObserved'7580'_344 :: T_LifecycleEvent'7580'_332
check_partialObserved'7580'_344 = PartialObserved
check_fanoutReverted'7580'_346 :: T_LifecycleEvent'7580'_332
check_fanoutReverted'7580'_346 = FanoutReverted
check_fannedOut'7580'_348 :: T_LifecycleEvent'7580'_332
check_fannedOut'7580'_348 = FannedOut
check_unchanged'7580'_350 :: T_LifecycleEvent'7580'_332
check_unchanged'7580'_350 = Unchanged
cover_LifecycleEvent'7580'_332 :: HsLifecycleEvent -> ()
cover_LifecycleEvent'7580'_332 x
  = case x of
      Opened -> ()
      Closed -> ()
      Contested -> ()
      Ready -> ()
      FanoutStarted -> ()
      PartialObserved -> ()
      FanoutReverted -> ()
      FannedOut -> ()
      Unchanged -> ()
-- Hydra.Protocol.OffChainReference.lifecyclePhaseRef
hsLifecyclePhaseRef ::
  T_LifecyclePhase'7580'_320 ->
  T_LifecycleEvent'7580'_332 -> T_LifecyclePhase'7580'_320
hsLifecyclePhaseRef = coe d_lifecyclePhaseRef_352
d_lifecyclePhaseRef_352 ::
  T_LifecyclePhase'7580'_320 ->
  T_LifecycleEvent'7580'_332 -> T_LifecyclePhase'7580'_320
d_lifecyclePhaseRef_352 v0 v1
  = case coe v0 of
      C_idleHead'7580'_322
        -> case coe v1 of
             C_opened'7580'_334 -> coe C_openHead'7580'_324
             _ -> coe v0
      C_openHead'7580'_324
        -> case coe v1 of
             C_closed'7580'_336 -> coe C_closedHead'7580'_326
             _ -> coe v0
      C_closedHead'7580'_326
        -> case coe v1 of
             C_contested'7580'_338 -> coe v0
             C_partialObserved'7580'_344 -> coe C_fanoutProgress'7580'_328
             C_fannedOut'7580'_348 -> coe C_finalizedHead'7580'_330
             _ -> coe v0
      C_fanoutProgress'7580'_328
        -> case coe v1 of
             C_fannedOut'7580'_348 -> coe C_finalizedHead'7580'_330
             _ -> coe v0
      _ -> coe v0
-- Hydra.Protocol.OffChainReference.CoordinationPhaseᶜ
d_CoordinationPhase'7580'_356 = ()
type T_CoordinationPhase'7580'_356 = HsCoordinationPhase
pattern C_outsideHead'7580'_358 = OutsideHead
pattern C_noRequestSeen'7580'_360 = NoRequestSeen
pattern C_snapshotIdle'7580'_362 = SnapshotIdle
pattern C_requestSent'7580'_364 = RequestSent
pattern C_collecting'7580'_366 = Collecting
check_outsideHead'7580'_358 :: T_CoordinationPhase'7580'_356
check_outsideHead'7580'_358 = OutsideHead
check_noRequestSeen'7580'_360 :: T_CoordinationPhase'7580'_356
check_noRequestSeen'7580'_360 = NoRequestSeen
check_snapshotIdle'7580'_362 :: T_CoordinationPhase'7580'_356
check_snapshotIdle'7580'_362 = SnapshotIdle
check_requestSent'7580'_364 :: T_CoordinationPhase'7580'_356
check_requestSent'7580'_364 = RequestSent
check_collecting'7580'_366 :: T_CoordinationPhase'7580'_356
check_collecting'7580'_366 = Collecting
cover_CoordinationPhase'7580'_356 :: HsCoordinationPhase -> ()
cover_CoordinationPhase'7580'_356 x
  = case x of
      OutsideHead -> ()
      NoRequestSeen -> ()
      SnapshotIdle -> ()
      RequestSent -> ()
      Collecting -> ()
-- Hydra.Protocol.OffChainReference.CoordinationEventᶜ
d_CoordinationEvent'7580'_368 = ()
type T_CoordinationEvent'7580'_368 = HsCoordinationEvent
pattern C_headOpened'7580'_370 = HeadOpened
pattern C_headClosed'7580'_372 = HeadClosed
pattern C_requestDecided'7580'_374 = RequestDecided
pattern C_snapshotRequested'7580'_376 = SnapshotRequested
pattern C_snapshotConfirmed'7580'_378 = SnapshotConfirmed
pattern C_localCleared'7580'_380 = LocalCleared
pattern C_settlementObserved'7580'_382 = SettlementObserved
pattern C_otherEvent'7580'_384 = OtherEvent
check_headOpened'7580'_370 :: T_CoordinationEvent'7580'_368
check_headOpened'7580'_370 = HeadOpened
check_headClosed'7580'_372 :: T_CoordinationEvent'7580'_368
check_headClosed'7580'_372 = HeadClosed
check_requestDecided'7580'_374 :: T_CoordinationEvent'7580'_368
check_requestDecided'7580'_374 = RequestDecided
check_snapshotRequested'7580'_376 :: T_CoordinationEvent'7580'_368
check_snapshotRequested'7580'_376 = SnapshotRequested
check_snapshotConfirmed'7580'_378 :: T_CoordinationEvent'7580'_368
check_snapshotConfirmed'7580'_378 = SnapshotConfirmed
check_localCleared'7580'_380 :: T_CoordinationEvent'7580'_368
check_localCleared'7580'_380 = LocalCleared
check_settlementObserved'7580'_382 :: T_CoordinationEvent'7580'_368
check_settlementObserved'7580'_382 = SettlementObserved
check_otherEvent'7580'_384 :: T_CoordinationEvent'7580'_368
check_otherEvent'7580'_384 = OtherEvent
cover_CoordinationEvent'7580'_368 :: HsCoordinationEvent -> ()
cover_CoordinationEvent'7580'_368 x
  = case x of
      HeadOpened -> ()
      HeadClosed -> ()
      RequestDecided -> ()
      SnapshotRequested -> ()
      SnapshotConfirmed -> ()
      LocalCleared -> ()
      SettlementObserved -> ()
      OtherEvent -> ()
-- Hydra.Protocol.OffChainReference.coordinationPhaseRef
hsCoordinationPhaseRef ::
  T_CoordinationPhase'7580'_356 ->
  T_CoordinationEvent'7580'_368 ->
  Bool -> Bool -> Bool -> T_CoordinationPhase'7580'_356
hsCoordinationPhaseRef = coe d_coordinationPhaseRef_386
d_coordinationPhaseRef_386 ::
  T_CoordinationPhase'7580'_356 ->
  T_CoordinationEvent'7580'_368 ->
  Bool -> Bool -> Bool -> T_CoordinationPhase'7580'_356
d_coordinationPhaseRef_386 v0 v1 v2 v3 v4
  = let v5
          = case coe v1 of
              C_headClosed'7580'_372 -> coe C_outsideHead'7580'_358
              C_requestDecided'7580'_374 -> coe C_requestSent'7580'_364
              C_snapshotRequested'7580'_376 -> coe C_collecting'7580'_366
              C_snapshotConfirmed'7580'_378
                -> coe
                     du_if_then_else__16 (coe v3) (coe C_snapshotIdle'7580'_362)
                     (coe v0)
              C_localCleared'7580'_380
                -> coe
                     du_if_then_else__16 (coe v2) (coe C_noRequestSeen'7580'_360)
                     (coe C_snapshotIdle'7580'_362)
              C_settlementObserved'7580'_382
                -> coe
                     du_if_then_else__16 (coe v4) (coe C_snapshotIdle'7580'_362)
                     (coe v0)
              _ -> coe v0 in
    coe
      (case coe v0 of
         C_outsideHead'7580'_358
           -> case coe v1 of
                C_headOpened'7580'_370 -> coe C_noRequestSeen'7580'_360
                C_headClosed'7580'_372 -> coe v0
                _ -> coe v0
         C_collecting'7580'_366
           -> case coe v1 of
                C_headClosed'7580'_372 -> coe C_outsideHead'7580'_358
                C_requestDecided'7580'_374 -> coe C_requestSent'7580'_364
                C_snapshotRequested'7580'_376 -> coe v0
                C_snapshotConfirmed'7580'_378 -> coe C_snapshotIdle'7580'_362
                C_localCleared'7580'_380
                  -> coe
                       du_if_then_else__16 (coe v2) (coe C_noRequestSeen'7580'_360)
                       (coe C_snapshotIdle'7580'_362)
                C_settlementObserved'7580'_382 -> coe v0
                _ -> coe v0
         _ -> coe v5)
-- Hydra.Protocol.OffChainReference.TrackedPhaseᶜ
d_TrackedPhase'7580'_400 = ()
type T_TrackedPhase'7580'_400 = HsTrackedPhase
pattern C_missingDeposit'7580'_402 = MissingDeposit
pattern C_inactiveDeposit'7580'_404 = InactiveDeposit
pattern C_activeDeposit'7580'_406 = ActiveDeposit
pattern C_expiredDeposit'7580'_408 = ExpiredDeposit
pattern C_consumedDeposit'7580'_410 = ConsumedDeposit
check_missingDeposit'7580'_402 :: T_TrackedPhase'7580'_400
check_missingDeposit'7580'_402 = MissingDeposit
check_inactiveDeposit'7580'_404 :: T_TrackedPhase'7580'_400
check_inactiveDeposit'7580'_404 = InactiveDeposit
check_activeDeposit'7580'_406 :: T_TrackedPhase'7580'_400
check_activeDeposit'7580'_406 = ActiveDeposit
check_expiredDeposit'7580'_408 :: T_TrackedPhase'7580'_400
check_expiredDeposit'7580'_408 = ExpiredDeposit
check_consumedDeposit'7580'_410 :: T_TrackedPhase'7580'_400
check_consumedDeposit'7580'_410 = ConsumedDeposit
cover_TrackedPhase'7580'_400 :: HsTrackedPhase -> ()
cover_TrackedPhase'7580'_400 x
  = case x of
      MissingDeposit -> ()
      InactiveDeposit -> ()
      ActiveDeposit -> ()
      ExpiredDeposit -> ()
      ConsumedDeposit -> ()
-- Hydra.Protocol.OffChainReference.statusPhase
d_statusPhase_412 ::
  T_DepositStatus'7580'_6 -> T_TrackedPhase'7580'_400
d_statusPhase_412 v0
  = case coe v0 of
      C_inactive'7580'_8 -> coe C_inactiveDeposit'7580'_404
      C_active'7580'_10 -> coe C_activeDeposit'7580'_406
      C_expired'7580'_12 -> coe C_expiredDeposit'7580'_408
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.trackedDepositPhaseRef
hsTrackedDepositPhaseRef ::
  T_TrackedPhase'7580'_400 ->
  Integer ->
  T_DepositStatus'7580'_6 -> Bool -> Bool -> T_TrackedPhase'7580'_400
hsTrackedDepositPhaseRef = coe d_trackedDepositPhaseRef_414
d_trackedDepositPhaseRef_414 ::
  T_TrackedPhase'7580'_400 ->
  Integer ->
  T_DepositStatus'7580'_6 -> Bool -> Bool -> T_TrackedPhase'7580'_400
d_trackedDepositPhaseRef_414 v0 v1 v2 v3 v4
  = case coe v1 of
      0 -> coe d_statusPhase_412 (coe v2)
      _ -> let v5
                 = case coe v1 of
                     1 -> coe d_statusPhase_412 (coe v2)
                     2 -> coe C_consumedDeposit'7580'_410
                     3 -> case coe v3 of
                            MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                              -> coe C_missingDeposit'7580'_402
                            _ -> coe v0
                     _ | coe geqInt (coe v1) (coe (3 :: Integer)) -> coe v0
                     _ -> coe v0 in
           coe
             (case coe v0 of
                C_missingDeposit'7580'_402 -> coe v0
                C_consumedDeposit'7580'_410
                  -> case coe v1 of
                       1 -> coe v0
                       2 -> coe v0
                       3 -> if coe v3
                              then coe C_missingDeposit'7580'_402
                              else (case coe v4 of
                                      MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                                        -> coe d_statusPhase_412 (coe v2)
                                      _ -> coe v5)
                       _ | coe geqInt (coe v1) (coe (3 :: Integer)) -> coe v5
                       _ -> coe v0
                _ -> coe v5)
-- Hydra.Protocol.OffChainReference.pendingIncrementPhaseRef
hsPendingIncrementPhaseRef :: Bool -> Bool -> Bool -> Integer
hsPendingIncrementPhaseRef = coe d_pendingIncrementPhaseRef_424
d_pendingIncrementPhaseRef_424 :: Bool -> Bool -> Bool -> Integer
d_pendingIncrementPhaseRef_424 v0 v1 v2
  = if coe v0
      then if coe v1
             then if coe v2 then coe (4 :: Integer) else coe (2 :: Integer)
             else (if coe v2 then coe (3 :: Integer) else coe (1 :: Integer))
      else coe (0 :: Integer)
-- Hydra.Protocol.OffChainReference.ReqDecOutcomeᶜ
d_ReqDecOutcome'7580'_426 = ()
type T_ReqDecOutcome'7580'_426 = HsReqDecOutcome
pattern C_decWaitCommit'7580'_428 = ReqDecWaitCommit
pattern C_decInvalidCommit'7580'_430 = ReqDecInvalidCommit
pattern C_decWaitExisting'7580'_432 = ReqDecWaitExisting
pattern C_decInvalidExisting'7580'_434 = ReqDecInvalidExisting
pattern C_decWaitLedger'7580'_436 = ReqDecWaitLedger
pattern C_decInvalidLedger'7580'_438 = ReqDecInvalidLedger
pattern C_decEmptyOutputs'7580'_440 = ReqDecEmptyOutputs
pattern C_decRecorded'7580'_442 = ReqDecRecorded
pattern C_decRequested'7580'_444 = ReqDecRequested
check_decWaitCommit'7580'_428 :: T_ReqDecOutcome'7580'_426
check_decWaitCommit'7580'_428 = ReqDecWaitCommit
check_decInvalidCommit'7580'_430 :: T_ReqDecOutcome'7580'_426
check_decInvalidCommit'7580'_430 = ReqDecInvalidCommit
check_decWaitExisting'7580'_432 :: T_ReqDecOutcome'7580'_426
check_decWaitExisting'7580'_432 = ReqDecWaitExisting
check_decInvalidExisting'7580'_434 :: T_ReqDecOutcome'7580'_426
check_decInvalidExisting'7580'_434 = ReqDecInvalidExisting
check_decWaitLedger'7580'_436 :: T_ReqDecOutcome'7580'_426
check_decWaitLedger'7580'_436 = ReqDecWaitLedger
check_decInvalidLedger'7580'_438 :: T_ReqDecOutcome'7580'_426
check_decInvalidLedger'7580'_438 = ReqDecInvalidLedger
check_decEmptyOutputs'7580'_440 :: T_ReqDecOutcome'7580'_426
check_decEmptyOutputs'7580'_440 = ReqDecEmptyOutputs
check_decRecorded'7580'_442 :: T_ReqDecOutcome'7580'_426
check_decRecorded'7580'_442 = ReqDecRecorded
check_decRequested'7580'_444 :: T_ReqDecOutcome'7580'_426
check_decRequested'7580'_444 = ReqDecRequested
cover_ReqDecOutcome'7580'_426 :: HsReqDecOutcome -> ()
cover_ReqDecOutcome'7580'_426 x
  = case x of
      ReqDecWaitCommit -> ()
      ReqDecInvalidCommit -> ()
      ReqDecWaitExisting -> ()
      ReqDecInvalidExisting -> ()
      ReqDecWaitLedger -> ()
      ReqDecInvalidLedger -> ()
      ReqDecEmptyOutputs -> ()
      ReqDecRecorded -> ()
      ReqDecRequested -> ()
-- Hydra.Protocol.OffChainReference.reqDecOutcomeRef
hsReqDecOutcomeRef ::
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqDecOutcome'7580'_426
hsReqDecOutcomeRef = coe d_reqDecOutcomeRef_446
d_reqDecOutcomeRef_446 ::
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqDecOutcome'7580'_426
d_reqDecOutcomeRef_446 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = if coe v0
      then case coe v4 of
             0 -> coe C_decInvalidCommit'7580'_430
             _ -> coe C_decWaitCommit'7580'_428
      else (if coe v1
              then case coe v4 of
                     0 -> coe C_decInvalidExisting'7580'_434
                     _ -> coe C_decWaitExisting'7580'_432
              else (if coe v2
                      then if coe v3
                             then coe
                                    du_if_then_else__16
                                    (coe
                                       d__'38''38'__34 (coe d_not_54 (coe v5))
                                       (coe d_leaderRef_152 (coe v6) (coe v7) (coe v8)))
                                    (coe C_decRequested'7580'_444) (coe C_decRecorded'7580'_442)
                             else coe C_decEmptyOutputs'7580'_440
                      else (case coe v4 of
                              0 -> coe C_decInvalidLedger'7580'_438
                              _ -> coe C_decWaitLedger'7580'_436)))
-- Hydra.Protocol.OffChainReference.RecoverOutcomeᶜ
d_RecoverOutcome'7580'_456 = ()
type T_RecoverOutcome'7580'_456 = HsRecoverOutcome
pattern C_recoveryMissing'7580'_458 = RecoveryMissing
pattern C_recoveryBlocked'7580'_460 = RecoveryBlocked
pattern C_recoveryPosted'7580'_462 = RecoveryPosted
check_recoveryMissing'7580'_458 :: T_RecoverOutcome'7580'_456
check_recoveryMissing'7580'_458 = RecoveryMissing
check_recoveryBlocked'7580'_460 :: T_RecoverOutcome'7580'_456
check_recoveryBlocked'7580'_460 = RecoveryBlocked
check_recoveryPosted'7580'_462 :: T_RecoverOutcome'7580'_456
check_recoveryPosted'7580'_462 = RecoveryPosted
cover_RecoverOutcome'7580'_456 :: HsRecoverOutcome -> ()
cover_RecoverOutcome'7580'_456 x
  = case x of
      RecoveryMissing -> ()
      RecoveryBlocked -> ()
      RecoveryPosted -> ()
-- Hydra.Protocol.OffChainReference.recoverOutcomeRef
hsRecoverOutcomeRef :: Bool -> Bool -> T_RecoverOutcome'7580'_456
hsRecoverOutcomeRef = coe d_recoverOutcomeRef_464
d_recoverOutcomeRef_464 ::
  Bool -> Bool -> T_RecoverOutcome'7580'_456
d_recoverOutcomeRef_464 v0 v1
  = if coe v0
      then if coe v1
             then coe C_recoveryBlocked'7580'_460
             else coe C_recoveryPosted'7580'_462
      else coe C_recoveryMissing'7580'_458
-- Hydra.Protocol.OffChainReference.inputAdmissionRef
hsInputAdmissionRef :: Integer -> Bool -> Bool -> Integer
hsInputAdmissionRef = coe d_inputAdmissionRef_466
d_inputAdmissionRef_466 :: Integer -> Bool -> Bool -> Integer
d_inputAdmissionRef_466 v0 v1 v2
  = let v3 = 0 :: Integer in
    coe
      (case coe v0 of
         0 -> if coe v1
                then case coe v2 of
                       MAlonzo.Code.Agda.Builtin.Bool.C_false_8 -> coe (2 :: Integer)
                       _ -> coe v3
                else coe (1 :: Integer)
         1 -> case coe v2 of
                MAlonzo.Code.Agda.Builtin.Bool.C_false_8 -> coe (3 :: Integer)
                _ -> coe v3
         _ -> coe v3)
-- Hydra.Protocol.OffChainReference.clientDecommitRef
hsClientDecommitRef :: Bool -> Bool -> Bool -> Integer
hsClientDecommitRef = coe d_clientDecommitRef_468
d_clientDecommitRef_468 :: Bool -> Bool -> Bool -> Integer
d_clientDecommitRef_468 v0 v1 v2
  = if coe v0
      then coe (0 :: Integer)
      else (if coe v1
              then if coe v2 then coe (3 :: Integer) else coe (2 :: Integer)
              else coe (1 :: Integer))
-- Hydra.Protocol.OffChainReference.adoptSettlementRef
hsAdoptSettlementRef ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool -> Bool
hsAdoptSettlementRef = coe d_adoptSettlementRef_470
d_adoptSettlementRef_470 ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool -> Bool
d_adoptSettlementRef_470 v0 v1 v2 v3 v4 v5
  = coe
      d__'38''38'__34 (coe eqInt (coe v0) (coe v1))
      (coe
         d__'38''38'__34
         (coe eqInt (coe addInt (coe (1 :: Integer)) (coe v2)) (coe v3))
         (coe d__'38''38'__34 (coe v4) (coe v5)))
-- Hydra.Protocol.OffChainReference.repostFinalizedRef
hsRepostFinalizedRef :: Bool -> Integer -> Integer -> Bool
hsRepostFinalizedRef = coe d_repostFinalizedRef_484
d_repostFinalizedRef_484 :: Bool -> Integer -> Integer -> Bool
d_repostFinalizedRef_484 v0 v1 v2
  = coe d__'124''124'__50 (coe v0) (coe ltInt (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.fanoutContinuationRef
hsFanoutContinuationRef :: Bool -> Integer -> Integer
hsFanoutContinuationRef = coe d_fanoutContinuationRef_492
d_fanoutContinuationRef_492 :: Bool -> Integer -> Integer
d_fanoutContinuationRef_492 v0 v1
  = if coe v0
      then coe (0 :: Integer)
      else (let v2 = 0 :: Integer in
            coe
              (case coe v1 of
                 0 -> coe (1 :: Integer)
                 1 -> coe (2 :: Integer)
                 _ -> coe v2))
-- Hydra.Protocol.OffChainReference.reservationHeldRef
hsReservationHeldRef ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsReservationHeldRef = coe d_reservationHeldRef_494
d_reservationHeldRef_494 :: [Integer] -> [Integer] -> Bool
d_reservationHeldRef_494 v0 v1
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v2 v3
        -> coe
             d__'38''38'__34
             (coe d_not_54 (coe d_elem'7495'_56 (coe v2) (coe v1)))
             (coe d_reservationHeldRef_494 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.decommitReadyRef
hsDecommitReadyRef ::
  Bool ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsDecommitReadyRef = coe d_decommitReadyRef_504
d_decommitReadyRef_504 :: Bool -> [Integer] -> [Integer] -> Bool
d_decommitReadyRef_504 v0 v1 v2
  = if coe v0
      then coe v0
      else (case coe v1 of
              [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
              (:) v3 v4
                -> coe
                     d__'38''38'__34 (coe d_elem'7495'_56 (coe v3) (coe v2))
                     (coe d_decommitReadyRef_504 (coe v0) (coe v4) (coe v2))
              _ -> MAlonzo.RTE.mazUnreachableError)
-- Hydra.Protocol.OffChainReference.settlementVersionRef
hsSettlementVersionRef :: Integer -> Integer -> Integer
hsSettlementVersionRef = coe d_settlementVersionRef_518
d_settlementVersionRef_518 :: Integer -> Integer -> Integer
d_settlementVersionRef_518 v0 v1
  = coe
      du_if_then_else__16 (coe ltInt (coe v0) (coe v1)) (coe v1) (coe v0)
-- Hydra.Protocol.OffChainReference.settlementReservationRef
hsSettlementReservationRef :: Bool -> Integer -> Integer -> Bool
hsSettlementReservationRef = coe d_settlementReservationRef_524
d_settlementReservationRef_524 ::
  Bool -> Integer -> Integer -> Bool
d_settlementReservationRef_524 v0 v1 v2
  = coe
      d__'38''38'__34 (coe v0)
      (coe d_not_54 (coe ltInt (coe v1) (coe v2)))
-- Hydra.Protocol.OffChainReference.reserveRebaseRef
hsReserveRebaseRef ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer
hsReserveRebaseRef = coe d_reserveRebaseRef_532
d_reserveRebaseRef_532 :: [Integer] -> [Integer] -> [Integer]
d_reserveRebaseRef_532 v0 v1
  = case coe v1 of
      [] -> coe v1
      (:) v2 v3
        -> coe
             du_if_then_else__16 (coe d_elem'7495'_56 (coe v2) (coe v0))
             (coe d_reserveRebaseRef_532 (coe v0) (coe v3))
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2)
                (coe d_reserveRebaseRef_532 (coe v0) (coe v3)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.newDecommitReadyRef
hsNewDecommitReadyRef ::
  Bool ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsNewDecommitReadyRef = coe d_newDecommitReadyRef_542
d_newDecommitReadyRef_542 :: Bool -> [Integer] -> [Integer] -> Bool
d_newDecommitReadyRef_542 v0 v1 v2
  = coe
      d__'38''38'__34 (coe d_not_54 (coe v0))
      (coe
         d_decommitReadyRef_504
         (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.accumulatorCapacityRef
hsAccumulatorCapacityRef :: Integer
hsAccumulatorCapacityRef = coe d_accumulatorCapacityRef_550
d_accumulatorCapacityRef_550 :: Integer
d_accumulatorCapacityRef_550 = coe (32767 :: Integer)
-- Hydra.Protocol.OffChainReference.snapshotCapacityRef
hsSnapshotCapacityRef :: Integer -> Integer -> Bool
hsSnapshotCapacityRef = coe d_snapshotCapacityRef_552
d_snapshotCapacityRef_552 :: Integer -> Integer -> Bool
d_snapshotCapacityRef_552 v0 v1
  = coe
      d__'38''38'__34
      (coe
         ltInt (coe v0)
         (coe
            addInt (coe (1 :: Integer)) (coe d_accumulatorCapacityRef_550)))
      (coe
         ltInt (coe v1)
         (coe
            addInt (coe (1 :: Integer)) (coe d_accumulatorCapacityRef_550)))
-- Hydra.Protocol.OffChainReference.decommitCapacityRef
hsDecommitCapacityRef :: Integer -> Integer -> Integer -> Bool
hsDecommitCapacityRef = coe d_decommitCapacityRef_558
d_decommitCapacityRef_558 :: Integer -> Integer -> Integer -> Bool
d_decommitCapacityRef_558 v0 v1 v2
  = coe
      d__'38''38'__34 (coe ltInt (coe (0 :: Integer)) (coe v0))
      (coe d_snapshotCapacityRef_552 (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.pendingTxFitsRef
hsPendingTxFitsRef :: Bool -> Bool -> Integer -> Integer -> Bool
hsPendingTxFitsRef = coe d_pendingTxFitsRef_566
d_pendingTxFitsRef_566 ::
  Bool -> Bool -> Integer -> Integer -> Bool
d_pendingTxFitsRef_566 v0 v1 v2 v3
  = coe
      d__'38''38'__34 (coe v0)
      (coe
         d__'38''38'__34 (coe v1)
         (coe d_snapshotCapacityRef_552 (coe v2) (coe v3)))
-- Hydra.Protocol.OffChainReference.capacityUnique
d_capacityUnique_576 :: [Integer] -> [Integer]
d_capacityUnique_576 v0
  = case coe v0 of
      [] -> coe v0
      (:) v1 v2
        -> coe
             du_if_then_else__16 (coe d_elem'7495'_56 (coe v1) (coe v2))
             (coe d_capacityUnique_576 (coe v2))
             (coe
                MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v1)
                (coe d_capacityUnique_576 (coe v2)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.capacityUnion
d_capacityUnion_582 :: [Integer] -> [Integer] -> [Integer]
d_capacityUnion_582 v0 v1
  = case coe v0 of
      [] -> coe d_capacityUnique_576 (coe v1)
      (:) v2 v3
        -> coe
             d_capacityUnion_582 (coe v3)
             (coe
                du_if_then_else__16 (coe d_elem'7495'_56 (coe v2) (coe v1))
                (coe v1)
                (coe
                   MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v2) (coe v1)))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.capacityLength
d_capacityLength_592 :: [Integer] -> Integer
d_capacityLength_592 v0
  = case coe v0 of
      [] -> coe (0 :: Integer)
      (:) v1 v2
        -> coe
             addInt (coe (1 :: Integer)) (coe d_capacityLength_592 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.capacityFitsRef
hsCapacityFitsRef ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsCapacityFitsRef = coe d_capacityFitsRef_596
d_capacityFitsRef_596 ::
  Integer -> [Integer] -> [Integer] -> [Integer] -> Bool
d_capacityFitsRef_596 v0 v1 v2 v3
  = coe
      d__'38''38'__34
      (coe
         ltInt
         (coe
            d_capacityLength_592 (coe d_capacityUnion_582 (coe v1) (coe v3)))
         (coe addInt (coe (1 :: Integer)) (coe v0)))
      (coe
         ltInt
         (coe
            d_capacityLength_592 (coe d_capacityUnion_582 (coe v1) (coe v2)))
         (coe addInt (coe (1 :: Integer)) (coe v0)))
-- Hydra.Protocol.OffChainReference.capacityApply
d_capacityApply_606 ::
  [Integer] -> [Integer] -> [Integer] -> [Integer]
d_capacityApply_606 v0 v1 v2
  = coe
      d_capacityUnion_582 (coe d_reserveRebaseRef_532 (coe v1) (coe v0))
      (coe v2)
-- Hydra.Protocol.OffChainReference.capacityPrefixRef
hsCapacityPrefixRef ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () (MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer) ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () (MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer) ->
  Integer
hsCapacityPrefixRef = coe d_capacityPrefixRef_614
d_capacityPrefixRef_614 ::
  Integer ->
  [Integer] -> [Integer] -> [[Integer]] -> [[Integer]] -> Integer
d_capacityPrefixRef_614 v0 v1 v2
  = coe d_go_626 (coe v0) (coe v1) (coe v2) (coe v1)
-- Hydra.Protocol.OffChainReference._.go
d_go_626 ::
  Integer ->
  [Integer] ->
  [Integer] -> [Integer] -> [[Integer]] -> [[Integer]] -> Integer
d_go_626 v0 v1 v2 v3 v4 v5
  = let v6 = 0 :: Integer in
    coe
      (case coe v4 of
         (:) v7 v8
           -> case coe v5 of
                (:) v9 v10
                  -> coe
                       du_if_then_else__16
                       (coe
                          d__'38''38'__34
                          (coe
                             d_reservationHeldRef_494 (coe v9)
                             (coe d_capacityUnion_582 (coe v1) (coe v2)))
                          (coe
                             d_capacityFitsRef_596 (coe v0)
                             (coe d_capacityApply_606 (coe v3) (coe v7) (coe v9)) (coe v2)
                             (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                       (coe
                          addInt (coe (1 :: Integer))
                          (coe
                             d_go_626 (coe v0) (coe v1) (coe v2)
                             (coe d_capacityApply_606 (coe v3) (coe v7) (coe v9)) (coe v8)
                             (coe v10)))
                       (coe (0 :: Integer))
                _ -> coe v6
         _ -> coe v6)
-- Hydra.Protocol.OffChainReference.capacityPruneRef
hsCapacityPruneRef ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () (MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer) ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () (MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer) ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer
hsCapacityPruneRef = coe d_capacityPruneRef_638
d_capacityPruneRef_638 ::
  Integer ->
  [Integer] ->
  [Integer] -> [Integer] -> [[Integer]] -> [[Integer]] -> [Integer]
d_capacityPruneRef_638 v0 v1 v2 v3
  = coe
      du_go_652 (coe v0) (coe v2) (coe v3) (coe (1 :: Integer)) (coe v1)
-- Hydra.Protocol.OffChainReference._.go
d_go_652 ::
  Integer ->
  [Integer] ->
  [Integer] ->
  [Integer] ->
  Integer -> [Integer] -> [[Integer]] -> [[Integer]] -> [Integer]
d_go_652 v0 ~v1 v2 v3 v4 v5 v6 v7 = du_go_652 v0 v2 v3 v4 v5 v6 v7
du_go_652 ::
  Integer ->
  [Integer] ->
  [Integer] ->
  Integer -> [Integer] -> [[Integer]] -> [[Integer]] -> [Integer]
du_go_652 v0 v1 v2 v3 v4 v5 v6
  = let v7 = coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16 in
    coe
      (case coe v5 of
         (:) v8 v9
           -> case coe v6 of
                (:) v10 v11
                  -> coe
                       du_if_then_else__16
                       (coe
                          d__'38''38'__34
                          (coe
                             d_decommitReadyRef_504
                             (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) (coe v8) (coe v4))
                          (coe
                             d__'38''38'__34
                             (coe
                                d_reservationHeldRef_494 (coe v10)
                                (coe d_capacityUnion_582 (coe v4) (coe v1)))
                             (coe
                                d_capacityFitsRef_596 (coe v0)
                                (coe d_capacityApply_606 (coe v4) (coe v8) (coe v10)) (coe v1)
                                (coe v2))))
                       (coe
                          MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v3)
                          (coe
                             du_go_652 (coe v0) (coe v1) (coe v2)
                             (coe addInt (coe (1 :: Integer)) (coe v3))
                             (coe d_capacityApply_606 (coe v4) (coe v8) (coe v10)) (coe v9)
                             (coe v11)))
                       (coe
                          du_go_652 (coe v0) (coe v1) (coe v2)
                          (coe addInt (coe (1 :: Integer)) (coe v3)) (coe v4) (coe v9)
                          (coe v11))
                _ -> coe v7
         _ -> coe v7)
-- Hydra.Protocol.OffChainReference.DecommitPhaseᶜ
d_DecommitPhase'7580'_666 = ()
data T_DecommitPhase'7580'_666
  = C_noDecommit'7580'_668 | C_pendingDecommit'7580'_670 |
    C_awaitingDecommitSettlement'7580'_672
-- Hydra.Protocol.OffChainReference.DecommitTransitionᶜ
d_DecommitTransition'7580'_674 a0 a1 = ()
data T_DecommitTransition'7580'_674
  = C_unchanged'7496'_678 | C_recordPending'7496'_680 |
    C_invalidate'7496'_682 | C_confirm'7496'_684 | C_adopt'7496'_686 |
    C_settle'7496'_688
-- Hydra.Protocol.OffChainReference.ImpossibleDecommitTransition
d_ImpossibleDecommitTransition_690 = ()
data T_ImpossibleDecommitTransition_690
-- Hydra.Protocol.OffChainReference.cannotReplaceAuthorisedDecommit
d_cannotReplaceAuthorisedDecommit_692 ::
  T_DecommitTransition'7580'_674 ->
  T_ImpossibleDecommitTransition_690
d_cannotReplaceAuthorisedDecommit_692 = erased
-- Hydra.Protocol.OffChainReference.retainReservationRef
hsRetainReservationRef ::
  Bool ->
  Bool ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsRetainReservationRef = coe d_retainReservationRef_694
d_retainReservationRef_694 ::
  Bool -> Bool -> [Integer] -> [Integer] -> Bool
d_retainReservationRef_694 v0 v1 v2 v3
  = coe
      d__'124''124'__50 (coe v0)
      (coe
         d__'38''38'__34 (coe d_not_54 (coe v1))
         (coe
            d_decommitReadyRef_504
            (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) (coe v2) (coe v3)))
-- Hydra.Protocol.OffChainReference.decommitAdmissionRef
hsDecommitAdmissionRef :: Bool -> Bool -> Bool
hsDecommitAdmissionRef = coe d_decommitAdmissionRef_704
d_decommitAdmissionRef_704 :: Bool -> Bool -> Bool
d_decommitAdmissionRef_704 v0 v1
  = coe d_not_54 (coe d__'124''124'__50 (coe v0) (coe v1))
-- Hydra.Protocol.OffChainReference.sameParties
d_sameParties_710 :: [Integer] -> [Integer] -> Bool
d_sameParties_710 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         []
           -> case coe v1 of
                [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                _ -> coe v2
         (:) v3 v4
           -> case coe v1 of
                (:) v5 v6
                  -> coe
                       d__'38''38'__34 (coe eqInt (coe v3) (coe v5))
                       (coe d_sameParties_710 (coe v4) (coe v6))
                _ -> coe v2
         _ -> MAlonzo.RTE.mazUnreachableError)
-- Hydra.Protocol.OffChainReference.verificationContextRef
hsVerificationContextRef ::
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsVerificationContextRef = coe d_verificationContextRef_720
d_verificationContextRef_720 ::
  Integer -> Integer -> [Integer] -> [Integer] -> Bool
d_verificationContextRef_720 v0 v1 v2 v3
  = coe
      d__'38''38'__34 (coe eqInt (coe v0) (coe v1))
      (coe d_sameParties_710 (coe v2) (coe v3))
-- Hydra.Protocol.OffChainReference.sameDecommitOutputs
d_sameDecommitOutputs_730 :: [Integer] -> [Integer] -> Bool
d_sameDecommitOutputs_730 v0 v1
  = let v2
          = d__'38''38'__34
              (coe
                 d_decommitReadyRef_504
                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) (coe v0) (coe v1))
              (coe
                 d_decommitReadyRef_504
                 (coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8) (coe v1)
                 (coe v0)) in
    coe
      (case coe v0 of
         [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
         _ -> coe v2)
-- Hydra.Protocol.OffChainReference.findDecommitPayload
d_findDecommitPayload_736 ::
  Integer -> [Integer] -> [[Integer]] -> Maybe Integer
d_findDecommitPayload_736 v0 v1 v2
  = case coe v2 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
      (:) v3 v4
        -> coe
             du_if_then_else__16
             (coe d_sameDecommitOutputs_730 (coe v1) (coe v3))
             (coe MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe v0))
             (coe
                d_findDecommitPayload_736
                (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1) (coe v4))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.carriedDecommitPayloadRef
hsCarriedDecommitPayloadRef ::
  Bool ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () (MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer) ->
  MAlonzo.Code.Agda.Builtin.Maybe.T_Maybe_10 () Integer
hsCarriedDecommitPayloadRef = coe d_carriedDecommitPayloadRef_750
d_carriedDecommitPayloadRef_750 ::
  Bool -> [Integer] -> [Integer] -> [[Integer]] -> Maybe Integer
d_carriedDecommitPayloadRef_750 v0 v1 v2 v3
  = if coe v0
      then coe
             du_if_then_else__16
             (coe d_sameDecommitOutputs_730 (coe v1) (coe v2))
             (coe
                MAlonzo.Code.Agda.Builtin.Maybe.C_just_16 (coe (0 :: Integer)))
             (coe
                d_findDecommitPayload_736 (coe (1 :: Integer)) (coe v1) (coe v3))
      else coe MAlonzo.Code.Agda.Builtin.Maybe.C_nothing_18
-- Hydra.Protocol.OffChainReference.carriedDecommitRequestRef
hsCarriedDecommitRequestRef :: Bool -> Bool -> Bool -> Bool
hsCarriedDecommitRequestRef = coe d_carriedDecommitRequestRef_758
d_carriedDecommitRequestRef_758 :: Bool -> Bool -> Bool -> Bool
d_carriedDecommitRequestRef_758 v0 v1 v2
  = coe
      d__'124''124'__50
      (coe d_not_54 (coe d__'38''38'__34 (coe v0) (coe v1))) (coe v2)
-- Hydra.Protocol.OffChainReference.reqSn-before-reqDec-retains-payload
d_reqSn'45'before'45'reqDec'45'retains'45'payload_766 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reqSn'45'before'45'reqDec'45'retains'45'payload_766 = erased
-- Hydra.Protocol.OffChainReference.settlement-does-not-revive-payload
d_settlement'45'does'45'not'45'revive'45'payload_774 ::
  [Integer] ->
  [Integer] ->
  [[Integer]] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_settlement'45'does'45'not'45'revive'45'payload_774 = erased
-- Hydra.Protocol.OffChainReference.missing-carried-payload-blocks-request
d_missing'45'carried'45'payload'45'blocks'45'request_776 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_missing'45'carried'45'payload'45'blocks'45'request_776 = erased
-- Hydra.Protocol.OffChainReference.reqTx-wait-retains-receipt
d_reqTx'45'wait'45'retains'45'receipt_778 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reqTx'45'wait'45'retains'45'receipt_778 = erased
-- Hydra.Protocol.OffChainReference.invalid-final-ack-discards-staged-signature
d_invalid'45'final'45'ack'45'discards'45'staged'45'signature_780 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_invalid'45'final'45'ack'45'discards'45'staged'45'signature_780
  = erased
-- Hydra.Protocol.OffChainReference.client-decommit-checks-ledger-before-empty-outputs
d_client'45'decommit'45'checks'45'ledger'45'before'45'empty'45'outputs_782 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_client'45'decommit'45'checks'45'ledger'45'before'45'empty'45'outputs_782
  = erased
-- Hydra.Protocol.OffChainReference.reqDec-ledger-failure-waits-with-budget
d_reqDec'45'ledger'45'failure'45'waits'45'with'45'budget_784 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reqDec'45'ledger'45'failure'45'waits'45'with'45'budget_784
  = erased
-- Hydra.Protocol.OffChainReference.settlement-invalid-signatures-prevent-adoption
d_settlement'45'invalid'45'signatures'45'prevent'45'adoption_786 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_settlement'45'invalid'45'signatures'45'prevent'45'adoption_786
  = erased
-- Hydra.Protocol.OffChainReference.settlement-still-advances-version
d_settlement'45'still'45'advances'45'version_788 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_settlement'45'still'45'advances'45'version_788 = erased
-- Hydra.Protocol.OffChainReference.SnapshotOwnerPhaseᶜ
d_SnapshotOwnerPhase'7580'_790 = ()
data T_SnapshotOwnerPhase'7580'_790
  = C_availableOwner'7580'_792 | C_requestedOwner'7580'_794 |
    C_collectingOwner'7580'_796
-- Hydra.Protocol.OffChainReference.SnapshotOwnerTransitionᶜ
d_SnapshotOwnerTransition'7580'_798 a0 a1 = ()
data T_SnapshotOwnerTransition'7580'_798
  = C_unchanged'738'_802 | C_request'738'_804 | C_receive'738'_806 |
    C_accept'738'_808 | C_resetRequest'738'_810 |
    C_finishCandidate'738'_812
-- Hydra.Protocol.OffChainReference.ImpossibleSnapshotTransition
d_ImpossibleSnapshotTransition_814 = ()
data T_ImpossibleSnapshotTransition_814
-- Hydra.Protocol.OffChainReference.cannotRestartCollecting
d_cannotRestartCollecting_816 ::
  T_SnapshotOwnerTransition'7580'_798 ->
  T_ImpossibleSnapshotTransition_814
d_cannotRestartCollecting_816 = erased
-- Hydra.Protocol.OffChainReference.liveSnapshotEdgeRef
hsLiveSnapshotEdgeRef :: Integer -> Integer -> Bool
hsLiveSnapshotEdgeRef = coe d_liveSnapshotEdgeRef_818
d_liveSnapshotEdgeRef_818 :: Integer -> Integer -> Bool
d_liveSnapshotEdgeRef_818 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         0 -> case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                1 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                2 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                _ -> coe v2
         1 -> case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                1 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                2 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                _ -> coe v2
         2 -> case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                2 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                _ | coe geqInt (coe v1) (coe (2 :: Integer)) -> coe v2
                _ -> coe v2
         _ -> coe v2)
-- Hydra.Protocol.OffChainReference.candidateIdentityRef
d_candidateIdentityRef_820 :: [Integer] -> [Integer] -> Bool
d_candidateIdentityRef_820 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         (:) v3 v4
           -> case coe v4 of
                (:) v5 v6
                  -> case coe v6 of
                       (:) v7 v8
                         -> case coe v8 of
                              (:) v9 v10
                                -> case coe v10 of
                                     []
                                       -> case coe v1 of
                                            (:) v11 v12
                                              -> case coe v12 of
                                                   (:) v13 v14
                                                     -> case coe v14 of
                                                          (:) v15 v16
                                                            -> case coe v16 of
                                                                 (:) v17 v18
                                                                   -> case coe v18 of
                                                                        []
                                                                          -> coe
                                                                               d__'38''38'__34
                                                                               (coe
                                                                                  eqInt (coe v3)
                                                                                  (coe v11))
                                                                               (coe
                                                                                  d__'38''38'__34
                                                                                  (coe
                                                                                     eqInt (coe v5)
                                                                                     (coe v13))
                                                                                  (coe
                                                                                     d__'38''38'__34
                                                                                     (coe
                                                                                        eqInt
                                                                                        (coe v7)
                                                                                        (coe v15))
                                                                                     (coe
                                                                                        eqInt
                                                                                        (coe v9)
                                                                                        (coe v17))))
                                                                        _ -> coe v2
                                                                 _ -> coe v2
                                                          _ -> coe v2
                                                   _ -> coe v2
                                            _ -> coe v2
                                     _ -> coe v2
                              _ -> coe v2
                       _ -> coe v2
                _ -> coe v2
         _ -> coe v2)
-- Hydra.Protocol.OffChainReference.candidateConfirmationRef
hsCandidateConfirmationRef ::
  Bool ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsCandidateConfirmationRef = coe d_candidateConfirmationRef_838
d_candidateConfirmationRef_838 ::
  Bool -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> Bool
d_candidateConfirmationRef_838 v0 v1 v2 v3 v4
  = coe
      d__'38''38'__34 (coe v0)
      (coe
         d__'38''38'__34 (coe d_candidateIdentityRef_820 (coe v1) (coe v2))
         (coe d_sameParties_710 (coe v3) (coe v4)))
-- Hydra.Protocol.OffChainReference.no-owner-no-confirmation
d_no'45'owner'45'no'45'confirmation_858 ::
  [Integer] ->
  [Integer] ->
  [Integer] ->
  [Integer] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_no'45'owner'45'no'45'confirmation_858 = erased
-- Hydra.Protocol.OffChainReference.another-round-is-not-this-candidate
d_another'45'round'45'is'45'not'45'this'45'candidate_860 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_another'45'round'45'is'45'not'45'this'45'candidate_860 = erased
-- Hydra.Protocol.OffChainReference.same-owned-candidate-confirms
d_same'45'owned'45'candidate'45'confirms_862 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_same'45'owned'45'candidate'45'confirms_862 = erased
-- Hydra.Protocol.OffChainReference.CommitPhaseᶜ
d_CommitPhase'7580'_864 = ()
type T_CommitPhase'7580'_864 = HsCommitPhase
pattern C_noCommit'7580'_866 = NoCommit
pattern C_selectedCommit'7580'_868 = CommitSelected
pattern C_awaitingCommitSettlement'7580'_870 = AwaitingCommitSettlement
check_noCommit'7580'_866 :: T_CommitPhase'7580'_864
check_noCommit'7580'_866 = NoCommit
check_selectedCommit'7580'_868 :: T_CommitPhase'7580'_864
check_selectedCommit'7580'_868 = CommitSelected
check_awaitingCommitSettlement'7580'_870 :: T_CommitPhase'7580'_864
check_awaitingCommitSettlement'7580'_870 = AwaitingCommitSettlement
cover_CommitPhase'7580'_864 :: HsCommitPhase -> ()
cover_CommitPhase'7580'_864 x
  = case x of
      NoCommit -> ()
      CommitSelected -> ()
      AwaitingCommitSettlement -> ()
-- Hydra.Protocol.OffChainReference.commitPhaseRef
hsCommitPhaseRef ::
  Bool -> Integer -> Integer -> Bool -> T_CommitPhase'7580'_864
hsCommitPhaseRef = coe d_commitPhaseRef_872
d_commitPhaseRef_872 ::
  Bool -> Integer -> Integer -> Bool -> T_CommitPhase'7580'_864
d_commitPhaseRef_872 v0 v1 v2 v3
  = coe
      du_if_then_else__16
      (coe d__'38''38'__34 (coe eqInt (coe v1) (coe v2)) (coe v3))
      (coe C_awaitingCommitSettlement'7580'_870)
      (coe
         du_if_then_else__16 (coe v0) (coe C_selectedCommit'7580'_868)
         (coe C_noCommit'7580'_866))
-- Hydra.Protocol.OffChainReference.adoption-without-selection-awaits
d_adoption'45'without'45'selection'45'awaits_882 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_adoption'45'without'45'selection'45'awaits_882 = erased
-- Hydra.Protocol.OffChainReference.settlement-before-confirmation-retires
d_settlement'45'before'45'confirmation'45'retires_884 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_settlement'45'before'45'confirmation'45'retires_884 = erased
-- Hydra.Protocol.OffChainReference.retirement-preserves-selection
d_retirement'45'preserves'45'selection_886 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_retirement'45'preserves'45'selection_886 = erased
-- Hydra.Protocol.OffChainReference.consumed-expiry-stays-consumed
d_consumed'45'expiry'45'stays'45'consumed_888 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_consumed'45'expiry'45'stays'45'consumed_888 = erased
-- Hydra.Protocol.OffChainReference.rollback-restores-expired
d_rollback'45'restores'45'expired_890 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_rollback'45'restores'45'expired_890 = erased
-- Hydra.Protocol.OffChainReference.contest-preserves-closed
d_contest'45'preserves'45'closed_892 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_contest'45'preserves'45'closed_892 = erased
-- Hydra.Protocol.OffChainReference.closed-does-not-reopen
d_closed'45'does'45'not'45'reopen_894 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_closed'45'does'45'not'45'reopen_894 = erased
-- Hydra.Protocol.OffChainReference.partial-does-not-reopen
d_partial'45'does'45'not'45'reopen_896 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_partial'45'does'45'not'45'reopen_896 = erased
-- Hydra.Protocol.OffChainReference.pending-remains-closed
d_pending'45'remains'45'closed_898 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_pending'45'remains'45'closed_898 = erased
-- Hydra.Protocol.OffChainReference.cancellation-preserves-closed
d_cancellation'45'preserves'45'closed_900 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_cancellation'45'preserves'45'closed_900 = erased
-- Hydra.Protocol.OffChainReference.partial-cannot-cancel
d_partial'45'cannot'45'cancel_902 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_partial'45'cannot'45'cancel_902 = erased
-- Hydra.Protocol.OffChainReference.observer-enters-partial
d_observer'45'enters'45'partial_904 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_observer'45'enters'45'partial_904 = erased
-- Hydra.Protocol.OffChainReference.full-fanout-finalizes
d_full'45'fanout'45'finalizes_906 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_full'45'fanout'45'finalizes_906 = erased
-- Hydra.Protocol.OffChainReference.partial-fanout-finalizes
d_partial'45'fanout'45'finalizes_908 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_partial'45'fanout'45'finalizes_908 = erased
-- Hydra.Protocol.OffChainReference.finalized-is-terminal
d_finalized'45'is'45'terminal_912 ::
  T_LifecycleEvent'7580'_332 ->
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_finalized'45'is'45'terminal_912 = erased
-- Hydra.Protocol.OffChainReference.reqDec-empty-outputs-do-not-wait
d_reqDec'45'empty'45'outputs'45'do'45'not'45'wait_916 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reqDec'45'empty'45'outputs'45'do'45'not'45'wait_916 = erased
-- Hydra.Protocol.OffChainReference.reqDec-expired-ledger-failure-is-invalid
d_reqDec'45'expired'45'ledger'45'failure'45'is'45'invalid_918 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_reqDec'45'expired'45'ledger'45'failure'45'is'45'invalid_918
  = erased
-- Hydra.Protocol.OffChainReference.effectiveDepositStatusRef
hsEffectiveDepositStatusRef ::
  T_DepositStatus'7580'_6 ->
  T_DepositStatus'7580'_6 -> T_DepositStatus'7580'_6
hsEffectiveDepositStatusRef = coe d_effectiveDepositStatusRef_920
d_effectiveDepositStatusRef_920 ::
  T_DepositStatus'7580'_6 ->
  T_DepositStatus'7580'_6 -> T_DepositStatus'7580'_6
d_effectiveDepositStatusRef_920 v0 v1
  = case coe v1 of
      C_inactive'7580'_8 -> coe v0
      C_active'7580'_10 -> coe v1
      C_expired'7580'_12 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.tick-does-not-erase-retained-activation
d_tick'45'does'45'not'45'erase'45'retained'45'activation_924 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_tick'45'does'45'not'45'erase'45'retained'45'activation_924
  = erased
-- Hydra.Protocol.OffChainReference.tick-does-not-erase-retained-expiry
d_tick'45'does'45'not'45'erase'45'retained'45'expiry_926 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_tick'45'does'45'not'45'erase'45'retained'45'expiry_926 = erased
-- Hydra.Protocol.OffChainReference.partial-ack-records-once
d_partial'45'ack'45'records'45'once_928 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_partial'45'ack'45'records'45'once_928 = erased
-- Hydra.Protocol.OffChainReference.settlementUTxORef
hsSettlementUTxORef ::
  Bool ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer
hsSettlementUTxORef = coe d_settlementUTxORef_930
d_settlementUTxORef_930 ::
  Bool -> Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
d_settlementUTxORef_930 v0 v1 v2 v3 v4
  = coe
      du_if_then_else__16
      (coe d__'38''38'__34 (coe v0) (coe ltInt (coe v1) (coe v2)))
      (coe v4) (coe v3)
-- Hydra.Protocol.OffChainReference.settlement-releases-unspent-reservation
d_settlement'45'releases'45'unspent'45'reservation_942 ::
  MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_settlement'45'releases'45'unspent'45'reservation_942 = erased
-- Hydra.Protocol.OffChainReference.settlement-without-reservation-preserves-ledger
d_settlement'45'without'45'reservation'45'preserves'45'ledger_952 ::
  Integer ->
  Integer ->
  [Integer] ->
  [Integer] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_settlement'45'without'45'reservation'45'preserves'45'ledger_952
  = erased
-- Hydra.Protocol.OffChainReference.repeated-settlement-preserves-new-reservation
d_repeated'45'settlement'45'preserves'45'new'45'reservation_958 ::
  [Integer] ->
  [Integer] -> MAlonzo.Code.Agda.Builtin.Equality.T__'8801'__12
d_repeated'45'settlement'45'preserves'45'new'45'reservation_958
  = erased
