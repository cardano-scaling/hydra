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
import qualified MAlonzo.Code.Agda.Builtin.List
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
data HsLifecyclePhase = IdleHead | OpenHead | ClosedHead | ReadyHead | PendingFanout | FanoutProgress deriving (Eq, Show, Enum, Bounded)
data HsLifecycleEvent = Opened | Closed | Ready | FanoutStarted | PartialObserved | FanoutReverted | FannedOut | Unchanged deriving (Eq, Show, Enum, Bounded)
data HsCoordinationPhase = OutsideHead | NoRequestSeen | SnapshotIdle | RequestSent | Collecting deriving (Eq, Show, Enum, Bounded)
data HsCoordinationEvent = HeadOpened | HeadClosed | RequestDecided | SnapshotRequested | SnapshotConfirmed | LocalCleared | SettlementObserved | OtherEvent deriving (Eq, Show, Enum, Bounded)
data HsTrackedPhase = MissingDeposit | InactiveDeposit | ActiveDeposit | ExpiredDeposit | ConsumedDeposit deriving (Eq, Show, Enum, Bounded)
data HsReqDecOutcome = ReqDecWaitCommit | ReqDecInvalidCommit
   | ReqDecWaitExisting | ReqDecInvalidExisting | ReqDecWaitLedger
   | ReqDecInvalidLedger | ReqDecEmptyOutputs | ReqDecRecorded
   | ReqDecRequested deriving (Eq, Show)
data HsRecoverOutcome = RecoveryMissing | RecoveryBlocked | RecoveryPosted deriving (Eq, Show)
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
-- Hydra.Protocol.OffChainReference.accumulatorCapacityRef
hsAccumulatorCapacityRef :: Integer
hsAccumulatorCapacityRef = coe d_accumulatorCapacityRef_206
d_accumulatorCapacityRef_206 :: Integer
d_accumulatorCapacityRef_206 = coe (32767 :: Integer)
-- Hydra.Protocol.OffChainReference.snapshotCapacityRef
hsSnapshotCapacityRef :: Integer -> Integer -> Bool
hsSnapshotCapacityRef = coe d_snapshotCapacityRef_208
d_snapshotCapacityRef_208 :: Integer -> Integer -> Bool
d_snapshotCapacityRef_208 v0 v1
  = coe
      d__'38''38'__34
      (coe
         ltInt (coe v0)
         (coe
            addInt (coe (1 :: Integer)) (coe d_accumulatorCapacityRef_206)))
      (coe
         ltInt (coe v1)
         (coe
            addInt (coe (1 :: Integer)) (coe d_accumulatorCapacityRef_206)))
-- Hydra.Protocol.OffChainReference.decommitCapacityRef
hsDecommitCapacityRef :: Integer -> Integer -> Integer -> Bool
hsDecommitCapacityRef = coe d_decommitCapacityRef_214
d_decommitCapacityRef_214 :: Integer -> Integer -> Integer -> Bool
d_decommitCapacityRef_214 v0 v1 v2
  = coe
      d__'38''38'__34 (coe ltInt (coe (0 :: Integer)) (coe v0))
      (coe d_snapshotCapacityRef_208 (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.pendingTxFitsRef
hsPendingTxFitsRef :: Bool -> Bool -> Integer -> Integer -> Bool
hsPendingTxFitsRef = coe d_pendingTxFitsRef_222
d_pendingTxFitsRef_222 ::
  Bool -> Bool -> Integer -> Integer -> Bool
d_pendingTxFitsRef_222 v0 v1 v2 v3
  = coe
      d__'38''38'__34 (coe v0)
      (coe
         d__'38''38'__34 (coe v1)
         (coe d_snapshotCapacityRef_208 (coe v2) (coe v3)))
-- Hydra.Protocol.OffChainReference.SnapshotPhaseᶜ
d_SnapshotPhase'7580'_232 = ()
type T_SnapshotPhase'7580'_232 = HsSnapshotPhase
pattern C_neverSeen'7580'_234 = NeverSeen
pattern C_requestPending'7580'_236 = RequestPending
pattern C_collectingSignatures'7580'_238 = CollectingSignatures
pattern C_lastConfirmed'7580'_240 = LastConfirmed
check_neverSeen'7580'_234 :: T_SnapshotPhase'7580'_232
check_neverSeen'7580'_234 = NeverSeen
check_requestPending'7580'_236 :: T_SnapshotPhase'7580'_232
check_requestPending'7580'_236 = RequestPending
check_collectingSignatures'7580'_238 :: T_SnapshotPhase'7580'_232
check_collectingSignatures'7580'_238 = CollectingSignatures
check_lastConfirmed'7580'_240 :: T_SnapshotPhase'7580'_232
check_lastConfirmed'7580'_240 = LastConfirmed
cover_SnapshotPhase'7580'_232 :: HsSnapshotPhase -> ()
cover_SnapshotPhase'7580'_232 x
  = case x of
      NeverSeen -> ()
      RequestPending -> ()
      CollectingSignatures -> ()
      LastConfirmed -> ()
-- Hydra.Protocol.OffChainReference.SnapshotChangeᶜ
d_SnapshotChange'7580'_242 = ()
type T_SnapshotChange'7580'_242 = HsSnapshotChange
pattern C_recordSignature'7580'_244 = RecordSignature
pattern C_confirmSnapshot'7580'_246 = ConfirmSnapshot
pattern C_decideNextSnapshot'7580'_248 = DecideNextSnapshot
pattern C_recordSnapshot'7580'_250 = RecordSnapshot
check_recordSignature'7580'_244 :: T_SnapshotChange'7580'_242
check_recordSignature'7580'_244 = RecordSignature
check_confirmSnapshot'7580'_246 :: T_SnapshotChange'7580'_242
check_confirmSnapshot'7580'_246 = ConfirmSnapshot
check_decideNextSnapshot'7580'_248 :: T_SnapshotChange'7580'_242
check_decideNextSnapshot'7580'_248 = DecideNextSnapshot
check_recordSnapshot'7580'_250 :: T_SnapshotChange'7580'_242
check_recordSnapshot'7580'_250 = RecordSnapshot
cover_SnapshotChange'7580'_242 :: HsSnapshotChange -> ()
cover_SnapshotChange'7580'_242 x
  = case x of
      RecordSignature -> ()
      ConfirmSnapshot -> ()
      DecideNextSnapshot -> ()
      RecordSnapshot -> ()
-- Hydra.Protocol.OffChainReference.SnapshotEffectᶜ
d_SnapshotEffect'7580'_252 = ()
type T_SnapshotEffect'7580'_252 = HsSnapshotEffect
pattern C_sendAcknowledgement'7580'_254 = SendAcknowledgement
pattern C_sendSnapshotRequest'7580'_256 = SendSnapshotRequest
check_sendAcknowledgement'7580'_254 :: T_SnapshotEffect'7580'_252
check_sendAcknowledgement'7580'_254 = SendAcknowledgement
check_sendSnapshotRequest'7580'_256 :: T_SnapshotEffect'7580'_252
check_sendSnapshotRequest'7580'_256 = SendSnapshotRequest
cover_SnapshotEffect'7580'_252 :: HsSnapshotEffect -> ()
cover_SnapshotEffect'7580'_252 x
  = case x of
      SendAcknowledgement -> ()
      SendSnapshotRequest -> ()
-- Hydra.Protocol.OffChainReference.SnapshotWaitᶜ
d_SnapshotWait'7580'_258 = ()
type T_SnapshotWait'7580'_258 = HsSnapshotWait
pattern C_previousUnconfirmed'7580'_260 = PreviousUnconfirmed
pattern C_versionMismatch'7580'_262 = VersionMismatch
pattern C_missingTransactions'7580'_264 = MissingTransactions
pattern C_snapshotNotSeen'7580'_266 = SnapshotNotSeen
check_previousUnconfirmed'7580'_260 :: T_SnapshotWait'7580'_258
check_previousUnconfirmed'7580'_260 = PreviousUnconfirmed
check_versionMismatch'7580'_262 :: T_SnapshotWait'7580'_258
check_versionMismatch'7580'_262 = VersionMismatch
check_missingTransactions'7580'_264 :: T_SnapshotWait'7580'_258
check_missingTransactions'7580'_264 = MissingTransactions
check_snapshotNotSeen'7580'_266 :: T_SnapshotWait'7580'_258
check_snapshotNotSeen'7580'_266 = SnapshotNotSeen
cover_SnapshotWait'7580'_258 :: HsSnapshotWait -> ()
cover_SnapshotWait'7580'_258 x
  = case x of
      PreviousUnconfirmed -> ()
      VersionMismatch -> ()
      MissingTransactions -> ()
      SnapshotNotSeen -> ()
-- Hydra.Protocol.OffChainReference.SnapshotRejectionᶜ
d_SnapshotRejection'7580'_268 = ()
type T_SnapshotRejection'7580'_268 = HsSnapshotRejection
pattern C_invalidNumber'7580'_270 = InvalidNumber
pattern C_notLeader'7580'_272 = NotLeader
pattern C_transactionDoesNotApply'7580'_274 = TransactionDoesNotApply
pattern C_alreadySigned'7580'_276 = AlreadySigned
pattern C_invalidSignatureSet'7580'_278 = InvalidSignatureSet
check_invalidNumber'7580'_270 :: T_SnapshotRejection'7580'_268
check_invalidNumber'7580'_270 = InvalidNumber
check_notLeader'7580'_272 :: T_SnapshotRejection'7580'_268
check_notLeader'7580'_272 = NotLeader
check_transactionDoesNotApply'7580'_274 ::
  T_SnapshotRejection'7580'_268
check_transactionDoesNotApply'7580'_274 = TransactionDoesNotApply
check_alreadySigned'7580'_276 :: T_SnapshotRejection'7580'_268
check_alreadySigned'7580'_276 = AlreadySigned
check_invalidSignatureSet'7580'_278 ::
  T_SnapshotRejection'7580'_268
check_invalidSignatureSet'7580'_278 = InvalidSignatureSet
cover_SnapshotRejection'7580'_268 :: HsSnapshotRejection -> ()
cover_SnapshotRejection'7580'_268 x
  = case x of
      InvalidNumber -> ()
      NotLeader -> ()
      TransactionDoesNotApply -> ()
      AlreadySigned -> ()
      InvalidSignatureSet -> ()
-- Hydra.Protocol.OffChainReference.SnapshotOutcomeᶜ
d_SnapshotOutcome'7580'_280 = ()
type T_SnapshotOutcome'7580'_280 = HsSnapshotOutcome
pattern C_snapshotCompleted'7580'_282 a0 a1 = SnapshotCompleted a0 a1
pattern C_snapshotWaiting'7580'_284 a0 = SnapshotWaiting a0
pattern C_snapshotRejected'7580'_286 a0 = SnapshotRejected a0
check_snapshotCompleted'7580'_282 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () T_SnapshotChange'7580'_242 ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10
    () T_SnapshotEffect'7580'_252 ->
  T_SnapshotOutcome'7580'_280
check_snapshotCompleted'7580'_282 = SnapshotCompleted
check_snapshotWaiting'7580'_284 ::
  T_SnapshotWait'7580'_258 -> T_SnapshotOutcome'7580'_280
check_snapshotWaiting'7580'_284 = SnapshotWaiting
check_snapshotRejected'7580'_286 ::
  T_SnapshotRejection'7580'_268 -> T_SnapshotOutcome'7580'_280
check_snapshotRejected'7580'_286 = SnapshotRejected
cover_SnapshotOutcome'7580'_280 :: HsSnapshotOutcome -> ()
cover_SnapshotOutcome'7580'_280 x
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
  Integer -> Integer -> Bool -> T_SnapshotOutcome'7580'_280
hsReqSnOutcomeRef = coe d_reqSnOutcomeRef_288
d_reqSnOutcomeRef_288 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Bool -> T_SnapshotOutcome'7580'_280
d_reqSnOutcomeRef_288 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      du_if_then_else__16
      (coe
         d_not_54
         (coe eqInt (coe v4) (coe addInt (coe (1 :: Integer)) (coe v0))))
      (coe C_snapshotRejected'7580'_286 (coe C_invalidNumber'7580'_270))
      (coe
         du_if_then_else__16
         (coe d_not_54 (coe d_leaderRef_152 (coe v5) (coe v4) (coe v6)))
         (coe C_snapshotRejected'7580'_286 (coe C_notLeader'7580'_272))
         (coe
            du_if_then_else__16 (coe d_not_54 (coe eqInt (coe v1) (coe v0)))
            (coe
               C_snapshotWaiting'7580'_284 (coe C_previousUnconfirmed'7580'_260))
            (coe
               du_if_then_else__16 (coe d_not_54 (coe eqInt (coe v2) (coe v3)))
               (coe C_snapshotWaiting'7580'_284 (coe C_versionMismatch'7580'_262))
               (coe
                  du_if_then_else__16 (coe ltInt (coe (0 :: Integer)) (coe v7))
                  (coe
                     C_snapshotWaiting'7580'_284 (coe C_missingTransactions'7580'_264))
                  (coe
                     du_if_then_else__16 (coe d_not_54 (coe v8))
                     (coe
                        C_snapshotRejected'7580'_286
                        (coe C_transactionDoesNotApply'7580'_274))
                     (coe
                        C_snapshotCompleted'7580'_282
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe C_recordSnapshot'7580'_250)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe C_sendAcknowledgement'7580'_254)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))))
-- Hydra.Protocol.OffChainReference.isLastConfirmed
d_isLastConfirmed_308 :: T_SnapshotPhase'7580'_232 -> Bool
d_isLastConfirmed_308 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         C_lastConfirmed'7580'_240
           -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
         _ -> coe v1)
-- Hydra.Protocol.OffChainReference.isCollecting
d_isCollecting_310 :: T_SnapshotPhase'7580'_232 -> Bool
d_isCollecting_310 v0
  = let v1 = coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8 in
    coe
      (case coe v0 of
         C_collectingSignatures'7580'_238
           -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
         _ -> coe v1)
-- Hydra.Protocol.OffChainReference.ackSnOutcomeRef
hsAckSnOutcomeRef ::
  T_SnapshotPhase'7580'_232 ->
  Integer ->
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  Integer ->
  Integer -> Bool -> Bool -> Integer -> T_SnapshotOutcome'7580'_280
hsAckSnOutcomeRef = coe d_ackSnOutcomeRef_312
d_ackSnOutcomeRef_312 ::
  T_SnapshotPhase'7580'_232 ->
  Integer ->
  Integer ->
  [Integer] ->
  Integer ->
  Integer -> Bool -> Bool -> Integer -> T_SnapshotOutcome'7580'_280
d_ackSnOutcomeRef_312 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = coe
      du_if_then_else__16
      (coe
         d_not_54
         (coe
            d__'124''124'__50 (coe eqInt (coe v2) (coe v1))
            (coe eqInt (coe v2) (coe addInt (coe (1 :: Integer)) (coe v1)))))
      (coe C_snapshotRejected'7580'_286 (coe C_invalidNumber'7580'_270))
      (coe
         du_if_then_else__16
         (coe
            d__'38''38'__34 (coe d_isLastConfirmed_308 (coe v0))
            (coe eqInt (coe v2) (coe v1)))
         (coe
            C_snapshotCompleted'7580'_282
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)
            (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
         (coe
            du_if_then_else__16
            (coe
               d_not_54
               (coe
                  d__'38''38'__34 (coe d_isCollecting_310 (coe v0))
                  (coe eqInt (coe v2) (coe v1))))
            (coe C_snapshotWaiting'7580'_284 (coe C_snapshotNotSeen'7580'_266))
            (coe
               du_if_then_else__16 (coe d_elem'7495'_56 (coe v4) (coe v3))
               (coe C_snapshotRejected'7580'_286 (coe C_alreadySigned'7580'_276))
               (coe
                  du_if_then_else__16
                  (coe
                     d_not_54
                     (coe
                        d_allSignedRef_134 (coe addInt (coe (1 :: Integer)) (coe v5))
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22 (coe v4) (coe v3))))
                  (coe
                     C_snapshotCompleted'7580'_282
                     (coe
                        MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                        (coe C_recordSignature'7580'_244)
                        (coe
                           MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                           (coe C_recordSignature'7580'_244)
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                     (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))
                  (coe
                     du_if_then_else__16 (coe d_not_54 (coe v6))
                     (coe
                        C_snapshotRejected'7580'_286 (coe C_invalidSignatureSet'7580'_278))
                     (coe
                        du_if_then_else__16
                        (coe
                           d__'38''38'__34 (coe v7)
                           (coe
                              d_leaderRef_152 (coe v5) (coe addInt (coe (1 :: Integer)) (coe v2))
                              (coe v8)))
                        (coe
                           C_snapshotCompleted'7580'_282
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe C_recordSignature'7580'_244)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                 (coe C_confirmSnapshot'7580'_246)
                                 (coe
                                    MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                    (coe C_decideNextSnapshot'7580'_248)
                                    (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe C_sendSnapshotRequest'7580'_256)
                              (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                        (coe
                           C_snapshotCompleted'7580'_282
                           (coe
                              MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                              (coe C_recordSignature'7580'_244)
                              (coe
                                 MAlonzo.Code.Agda.Builtin.List.C__'8759'__22
                                 (coe C_confirmSnapshot'7580'_246)
                                 (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16)))
                           (coe MAlonzo.Code.Agda.Builtin.List.C_'91''93'_16))))))))
-- Hydra.Protocol.OffChainReference.reqTxKnownOutcomeRef
hsReqTxKnownOutcomeRef ::
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqTxOutcome'7580'_180
hsReqTxKnownOutcomeRef = coe d_reqTxKnownOutcomeRef_332
d_reqTxKnownOutcomeRef_332 ::
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqTxOutcome'7580'_180
d_reqTxKnownOutcomeRef_332 v0 v1 v2 v3 v4 v5 v6
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
pattern C_readyHead'7580'_328 = ReadyHead
pattern C_pendingFanout'7580'_330 = PendingFanout
pattern C_fanoutProgress'7580'_332 = FanoutProgress
check_idleHead'7580'_322 :: T_LifecyclePhase'7580'_320
check_idleHead'7580'_322 = IdleHead
check_openHead'7580'_324 :: T_LifecyclePhase'7580'_320
check_openHead'7580'_324 = OpenHead
check_closedHead'7580'_326 :: T_LifecyclePhase'7580'_320
check_closedHead'7580'_326 = ClosedHead
check_readyHead'7580'_328 :: T_LifecyclePhase'7580'_320
check_readyHead'7580'_328 = ReadyHead
check_pendingFanout'7580'_330 :: T_LifecyclePhase'7580'_320
check_pendingFanout'7580'_330 = PendingFanout
check_fanoutProgress'7580'_332 :: T_LifecyclePhase'7580'_320
check_fanoutProgress'7580'_332 = FanoutProgress
cover_LifecyclePhase'7580'_320 :: HsLifecyclePhase -> ()
cover_LifecyclePhase'7580'_320 x
  = case x of
      IdleHead -> ()
      OpenHead -> ()
      ClosedHead -> ()
      ReadyHead -> ()
      PendingFanout -> ()
      FanoutProgress -> ()
-- Hydra.Protocol.OffChainReference.LifecycleEventᶜ
d_LifecycleEvent'7580'_334 = ()
type T_LifecycleEvent'7580'_334 = HsLifecycleEvent
pattern C_opened'7580'_336 = Opened
pattern C_closed'7580'_338 = Closed
pattern C_ready'7580'_340 = Ready
pattern C_fanoutStarted'7580'_342 = FanoutStarted
pattern C_partialObserved'7580'_344 = PartialObserved
pattern C_fanoutReverted'7580'_346 = FanoutReverted
pattern C_fannedOut'7580'_348 = FannedOut
pattern C_unchanged'7580'_350 = Unchanged
check_opened'7580'_336 :: T_LifecycleEvent'7580'_334
check_opened'7580'_336 = Opened
check_closed'7580'_338 :: T_LifecycleEvent'7580'_334
check_closed'7580'_338 = Closed
check_ready'7580'_340 :: T_LifecycleEvent'7580'_334
check_ready'7580'_340 = Ready
check_fanoutStarted'7580'_342 :: T_LifecycleEvent'7580'_334
check_fanoutStarted'7580'_342 = FanoutStarted
check_partialObserved'7580'_344 :: T_LifecycleEvent'7580'_334
check_partialObserved'7580'_344 = PartialObserved
check_fanoutReverted'7580'_346 :: T_LifecycleEvent'7580'_334
check_fanoutReverted'7580'_346 = FanoutReverted
check_fannedOut'7580'_348 :: T_LifecycleEvent'7580'_334
check_fannedOut'7580'_348 = FannedOut
check_unchanged'7580'_350 :: T_LifecycleEvent'7580'_334
check_unchanged'7580'_350 = Unchanged
cover_LifecycleEvent'7580'_334 :: HsLifecycleEvent -> ()
cover_LifecycleEvent'7580'_334 x
  = case x of
      Opened -> ()
      Closed -> ()
      Ready -> ()
      FanoutStarted -> ()
      PartialObserved -> ()
      FanoutReverted -> ()
      FannedOut -> ()
      Unchanged -> ()
-- Hydra.Protocol.OffChainReference.lifecyclePhaseRef
hsLifecyclePhaseRef ::
  T_LifecyclePhase'7580'_320 ->
  T_LifecycleEvent'7580'_334 -> Bool -> T_LifecyclePhase'7580'_320
hsLifecyclePhaseRef = coe d_lifecyclePhaseRef_352
d_lifecyclePhaseRef_352 ::
  T_LifecyclePhase'7580'_320 ->
  T_LifecycleEvent'7580'_334 -> Bool -> T_LifecyclePhase'7580'_320
d_lifecyclePhaseRef_352 v0 v1 v2
  = case coe v1 of
      C_opened'7580'_336 -> coe C_openHead'7580'_324
      C_closed'7580'_338
        -> case coe v0 of
             C_openHead'7580'_324 -> coe C_closedHead'7580'_326
             _ -> coe v0
      C_ready'7580'_340
        -> case coe v0 of
             C_closedHead'7580'_326 -> coe C_readyHead'7580'_328
             _ -> coe v0
      C_fanoutStarted'7580'_342
        -> case coe v0 of
             C_closedHead'7580'_326 -> coe C_pendingFanout'7580'_330
             C_readyHead'7580'_328 -> coe C_pendingFanout'7580'_330
             _ -> coe v0
      C_partialObserved'7580'_344
        -> case coe v0 of
             C_closedHead'7580'_326
               -> coe
                    du_if_then_else__16 (coe v2) (coe C_fanoutProgress'7580'_332)
                    (coe C_pendingFanout'7580'_330)
             C_readyHead'7580'_328
               -> coe
                    du_if_then_else__16 (coe v2) (coe C_fanoutProgress'7580'_332)
                    (coe C_pendingFanout'7580'_330)
             C_pendingFanout'7580'_330
               -> coe
                    du_if_then_else__16 (coe v2) (coe C_fanoutProgress'7580'_332)
                    (coe v0)
             C_fanoutProgress'7580'_332
               -> coe
                    du_if_then_else__16 (coe v2) (coe v0)
                    (coe C_pendingFanout'7580'_330)
             _ -> coe v0
      C_fanoutReverted'7580'_346
        -> case coe v0 of
             C_pendingFanout'7580'_330 -> coe C_readyHead'7580'_328
             C_fanoutProgress'7580'_332 -> coe C_readyHead'7580'_328
             _ -> coe v0
      C_fannedOut'7580'_348
        -> case coe v0 of
             C_closedHead'7580'_326 -> coe C_idleHead'7580'_322
             C_readyHead'7580'_328 -> coe C_idleHead'7580'_322
             C_pendingFanout'7580'_330 -> coe C_idleHead'7580'_322
             C_fanoutProgress'7580'_332 -> coe C_idleHead'7580'_322
             _ -> coe v0
      _ -> coe v0
-- Hydra.Protocol.OffChainReference.CoordinationPhaseᶜ
d_CoordinationPhase'7580'_364 = ()
type T_CoordinationPhase'7580'_364 = HsCoordinationPhase
pattern C_outsideHead'7580'_366 = OutsideHead
pattern C_noRequestSeen'7580'_368 = NoRequestSeen
pattern C_snapshotIdle'7580'_370 = SnapshotIdle
pattern C_requestSent'7580'_372 = RequestSent
pattern C_collecting'7580'_374 = Collecting
check_outsideHead'7580'_366 :: T_CoordinationPhase'7580'_364
check_outsideHead'7580'_366 = OutsideHead
check_noRequestSeen'7580'_368 :: T_CoordinationPhase'7580'_364
check_noRequestSeen'7580'_368 = NoRequestSeen
check_snapshotIdle'7580'_370 :: T_CoordinationPhase'7580'_364
check_snapshotIdle'7580'_370 = SnapshotIdle
check_requestSent'7580'_372 :: T_CoordinationPhase'7580'_364
check_requestSent'7580'_372 = RequestSent
check_collecting'7580'_374 :: T_CoordinationPhase'7580'_364
check_collecting'7580'_374 = Collecting
cover_CoordinationPhase'7580'_364 :: HsCoordinationPhase -> ()
cover_CoordinationPhase'7580'_364 x
  = case x of
      OutsideHead -> ()
      NoRequestSeen -> ()
      SnapshotIdle -> ()
      RequestSent -> ()
      Collecting -> ()
-- Hydra.Protocol.OffChainReference.CoordinationEventᶜ
d_CoordinationEvent'7580'_376 = ()
type T_CoordinationEvent'7580'_376 = HsCoordinationEvent
pattern C_headOpened'7580'_378 = HeadOpened
pattern C_headClosed'7580'_380 = HeadClosed
pattern C_requestDecided'7580'_382 = RequestDecided
pattern C_snapshotRequested'7580'_384 = SnapshotRequested
pattern C_snapshotConfirmed'7580'_386 = SnapshotConfirmed
pattern C_localCleared'7580'_388 = LocalCleared
pattern C_settlementObserved'7580'_390 = SettlementObserved
pattern C_otherEvent'7580'_392 = OtherEvent
check_headOpened'7580'_378 :: T_CoordinationEvent'7580'_376
check_headOpened'7580'_378 = HeadOpened
check_headClosed'7580'_380 :: T_CoordinationEvent'7580'_376
check_headClosed'7580'_380 = HeadClosed
check_requestDecided'7580'_382 :: T_CoordinationEvent'7580'_376
check_requestDecided'7580'_382 = RequestDecided
check_snapshotRequested'7580'_384 :: T_CoordinationEvent'7580'_376
check_snapshotRequested'7580'_384 = SnapshotRequested
check_snapshotConfirmed'7580'_386 :: T_CoordinationEvent'7580'_376
check_snapshotConfirmed'7580'_386 = SnapshotConfirmed
check_localCleared'7580'_388 :: T_CoordinationEvent'7580'_376
check_localCleared'7580'_388 = LocalCleared
check_settlementObserved'7580'_390 :: T_CoordinationEvent'7580'_376
check_settlementObserved'7580'_390 = SettlementObserved
check_otherEvent'7580'_392 :: T_CoordinationEvent'7580'_376
check_otherEvent'7580'_392 = OtherEvent
cover_CoordinationEvent'7580'_376 :: HsCoordinationEvent -> ()
cover_CoordinationEvent'7580'_376 x
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
  T_CoordinationPhase'7580'_364 ->
  T_CoordinationEvent'7580'_376 ->
  Bool -> Bool -> Bool -> T_CoordinationPhase'7580'_364
hsCoordinationPhaseRef = coe d_coordinationPhaseRef_394
d_coordinationPhaseRef_394 ::
  T_CoordinationPhase'7580'_364 ->
  T_CoordinationEvent'7580'_376 ->
  Bool -> Bool -> Bool -> T_CoordinationPhase'7580'_364
d_coordinationPhaseRef_394 v0 v1 v2 v3 v4
  = case coe v1 of
      C_headOpened'7580'_378 -> coe C_noRequestSeen'7580'_368
      C_headClosed'7580'_380 -> coe C_outsideHead'7580'_366
      C_requestDecided'7580'_382
        -> let v5 = coe C_requestSent'7580'_372 in
           coe
             (case coe v0 of
                C_outsideHead'7580'_366 -> coe v0
                _ -> coe v5)
      C_snapshotRequested'7580'_384
        -> let v5 = coe C_collecting'7580'_374 in
           coe
             (case coe v0 of
                C_outsideHead'7580'_366 -> coe v0
                _ -> coe v5)
      C_snapshotConfirmed'7580'_386
        -> let v5
                 = coe
                     du_if_then_else__16 (coe v3) (coe C_snapshotIdle'7580'_370)
                     (coe v0) in
           coe
             (case coe v0 of
                C_outsideHead'7580'_366 -> coe v0
                C_collecting'7580'_374 -> coe C_snapshotIdle'7580'_370
                _ -> coe v5)
      C_localCleared'7580'_388
        -> let v5
                 = coe
                     du_if_then_else__16 (coe v2) (coe C_noRequestSeen'7580'_368)
                     (coe C_snapshotIdle'7580'_370) in
           coe
             (case coe v0 of
                C_outsideHead'7580'_366 -> coe v0
                _ -> coe v5)
      C_settlementObserved'7580'_390
        -> let v5
                 = coe
                     du_if_then_else__16 (coe v4) (coe C_snapshotIdle'7580'_370)
                     (coe v0) in
           coe
             (case coe v0 of
                C_outsideHead'7580'_366 -> coe v0
                C_collecting'7580'_374 -> coe v0
                _ -> coe v5)
      _ -> coe v0
-- Hydra.Protocol.OffChainReference.TrackedPhaseᶜ
d_TrackedPhase'7580'_408 = ()
type T_TrackedPhase'7580'_408 = HsTrackedPhase
pattern C_missingDeposit'7580'_410 = MissingDeposit
pattern C_inactiveDeposit'7580'_412 = InactiveDeposit
pattern C_activeDeposit'7580'_414 = ActiveDeposit
pattern C_expiredDeposit'7580'_416 = ExpiredDeposit
pattern C_consumedDeposit'7580'_418 = ConsumedDeposit
check_missingDeposit'7580'_410 :: T_TrackedPhase'7580'_408
check_missingDeposit'7580'_410 = MissingDeposit
check_inactiveDeposit'7580'_412 :: T_TrackedPhase'7580'_408
check_inactiveDeposit'7580'_412 = InactiveDeposit
check_activeDeposit'7580'_414 :: T_TrackedPhase'7580'_408
check_activeDeposit'7580'_414 = ActiveDeposit
check_expiredDeposit'7580'_416 :: T_TrackedPhase'7580'_408
check_expiredDeposit'7580'_416 = ExpiredDeposit
check_consumedDeposit'7580'_418 :: T_TrackedPhase'7580'_408
check_consumedDeposit'7580'_418 = ConsumedDeposit
cover_TrackedPhase'7580'_408 :: HsTrackedPhase -> ()
cover_TrackedPhase'7580'_408 x
  = case x of
      MissingDeposit -> ()
      InactiveDeposit -> ()
      ActiveDeposit -> ()
      ExpiredDeposit -> ()
      ConsumedDeposit -> ()
-- Hydra.Protocol.OffChainReference.statusPhase
d_statusPhase_420 ::
  T_DepositStatus'7580'_6 -> T_TrackedPhase'7580'_408
d_statusPhase_420 v0
  = case coe v0 of
      C_inactive'7580'_8 -> coe C_inactiveDeposit'7580'_412
      C_active'7580'_10 -> coe C_activeDeposit'7580'_414
      C_expired'7580'_12 -> coe C_expiredDeposit'7580'_416
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.trackedDepositPhaseRef
hsTrackedDepositPhaseRef ::
  T_TrackedPhase'7580'_408 ->
  Integer ->
  T_DepositStatus'7580'_6 -> Bool -> Bool -> T_TrackedPhase'7580'_408
hsTrackedDepositPhaseRef = coe d_trackedDepositPhaseRef_422
d_trackedDepositPhaseRef_422 ::
  T_TrackedPhase'7580'_408 ->
  Integer ->
  T_DepositStatus'7580'_6 -> Bool -> Bool -> T_TrackedPhase'7580'_408
d_trackedDepositPhaseRef_422 v0 v1 v2 v3 v4
  = case coe v1 of
      0 -> coe d_statusPhase_420 (coe v2)
      _ -> let v5
                 = case coe v1 of
                     1 -> coe d_statusPhase_420 (coe v2)
                     2 -> coe C_consumedDeposit'7580'_418
                     3 -> case coe v3 of
                            MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                              -> coe C_missingDeposit'7580'_410
                            _ -> coe v0
                     _ | coe geqInt (coe v1) (coe (3 :: Integer)) -> coe v0
                     _ -> coe v0 in
           coe
             (case coe v0 of
                C_missingDeposit'7580'_410 -> coe v0
                C_consumedDeposit'7580'_418
                  -> case coe v1 of
                       1 -> coe v0
                       2 -> coe v0
                       3 -> if coe v3
                              then coe C_missingDeposit'7580'_410
                              else (case coe v4 of
                                      MAlonzo.Code.Agda.Builtin.Bool.C_true_10
                                        -> coe d_statusPhase_420 (coe v2)
                                      _ -> coe v5)
                       _ | coe geqInt (coe v1) (coe (3 :: Integer)) -> coe v5
                       _ -> coe v0
                _ -> coe v5)
-- Hydra.Protocol.OffChainReference.pendingIncrementPhaseRef
hsPendingIncrementPhaseRef :: Bool -> Bool -> Bool -> Integer
hsPendingIncrementPhaseRef = coe d_pendingIncrementPhaseRef_432
d_pendingIncrementPhaseRef_432 :: Bool -> Bool -> Bool -> Integer
d_pendingIncrementPhaseRef_432 v0 v1 v2
  = if coe v0
      then if coe v1
             then if coe v2 then coe (4 :: Integer) else coe (2 :: Integer)
             else (if coe v2 then coe (3 :: Integer) else coe (1 :: Integer))
      else coe (0 :: Integer)
-- Hydra.Protocol.OffChainReference.ReqDecOutcomeᶜ
d_ReqDecOutcome'7580'_434 = ()
type T_ReqDecOutcome'7580'_434 = HsReqDecOutcome
pattern C_decWaitCommit'7580'_436 = ReqDecWaitCommit
pattern C_decInvalidCommit'7580'_438 = ReqDecInvalidCommit
pattern C_decWaitExisting'7580'_440 = ReqDecWaitExisting
pattern C_decInvalidExisting'7580'_442 = ReqDecInvalidExisting
pattern C_decWaitLedger'7580'_444 = ReqDecWaitLedger
pattern C_decInvalidLedger'7580'_446 = ReqDecInvalidLedger
pattern C_decEmptyOutputs'7580'_448 = ReqDecEmptyOutputs
pattern C_decRecorded'7580'_450 = ReqDecRecorded
pattern C_decRequested'7580'_452 = ReqDecRequested
check_decWaitCommit'7580'_436 :: T_ReqDecOutcome'7580'_434
check_decWaitCommit'7580'_436 = ReqDecWaitCommit
check_decInvalidCommit'7580'_438 :: T_ReqDecOutcome'7580'_434
check_decInvalidCommit'7580'_438 = ReqDecInvalidCommit
check_decWaitExisting'7580'_440 :: T_ReqDecOutcome'7580'_434
check_decWaitExisting'7580'_440 = ReqDecWaitExisting
check_decInvalidExisting'7580'_442 :: T_ReqDecOutcome'7580'_434
check_decInvalidExisting'7580'_442 = ReqDecInvalidExisting
check_decWaitLedger'7580'_444 :: T_ReqDecOutcome'7580'_434
check_decWaitLedger'7580'_444 = ReqDecWaitLedger
check_decInvalidLedger'7580'_446 :: T_ReqDecOutcome'7580'_434
check_decInvalidLedger'7580'_446 = ReqDecInvalidLedger
check_decEmptyOutputs'7580'_448 :: T_ReqDecOutcome'7580'_434
check_decEmptyOutputs'7580'_448 = ReqDecEmptyOutputs
check_decRecorded'7580'_450 :: T_ReqDecOutcome'7580'_434
check_decRecorded'7580'_450 = ReqDecRecorded
check_decRequested'7580'_452 :: T_ReqDecOutcome'7580'_434
check_decRequested'7580'_452 = ReqDecRequested
cover_ReqDecOutcome'7580'_434 :: HsReqDecOutcome -> ()
cover_ReqDecOutcome'7580'_434 x
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
  Bool -> Integer -> Integer -> Integer -> T_ReqDecOutcome'7580'_434
hsReqDecOutcomeRef = coe d_reqDecOutcomeRef_454
d_reqDecOutcomeRef_454 ::
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Integer ->
  Bool -> Integer -> Integer -> Integer -> T_ReqDecOutcome'7580'_434
d_reqDecOutcomeRef_454 v0 v1 v2 v3 v4 v5 v6 v7 v8
  = if coe v0
      then case coe v4 of
             0 -> coe C_decInvalidCommit'7580'_438
             _ -> coe C_decWaitCommit'7580'_436
      else (if coe v1
              then case coe v4 of
                     0 -> coe C_decInvalidExisting'7580'_442
                     _ -> coe C_decWaitExisting'7580'_440
              else (if coe v2
                      then if coe v3
                             then coe
                                    du_if_then_else__16
                                    (coe
                                       d__'38''38'__34 (coe d_not_54 (coe v5))
                                       (coe d_leaderRef_152 (coe v6) (coe v7) (coe v8)))
                                    (coe C_decRequested'7580'_452) (coe C_decRecorded'7580'_450)
                             else coe C_decEmptyOutputs'7580'_448
                      else (case coe v4 of
                              0 -> coe C_decInvalidLedger'7580'_446
                              _ -> coe C_decWaitLedger'7580'_444)))
-- Hydra.Protocol.OffChainReference.RecoverOutcomeᶜ
d_RecoverOutcome'7580'_464 = ()
type T_RecoverOutcome'7580'_464 = HsRecoverOutcome
pattern C_recoveryMissing'7580'_466 = RecoveryMissing
pattern C_recoveryBlocked'7580'_468 = RecoveryBlocked
pattern C_recoveryPosted'7580'_470 = RecoveryPosted
check_recoveryMissing'7580'_466 :: T_RecoverOutcome'7580'_464
check_recoveryMissing'7580'_466 = RecoveryMissing
check_recoveryBlocked'7580'_468 :: T_RecoverOutcome'7580'_464
check_recoveryBlocked'7580'_468 = RecoveryBlocked
check_recoveryPosted'7580'_470 :: T_RecoverOutcome'7580'_464
check_recoveryPosted'7580'_470 = RecoveryPosted
cover_RecoverOutcome'7580'_464 :: HsRecoverOutcome -> ()
cover_RecoverOutcome'7580'_464 x
  = case x of
      RecoveryMissing -> ()
      RecoveryBlocked -> ()
      RecoveryPosted -> ()
-- Hydra.Protocol.OffChainReference.recoverOutcomeRef
hsRecoverOutcomeRef :: Bool -> Bool -> T_RecoverOutcome'7580'_464
hsRecoverOutcomeRef = coe d_recoverOutcomeRef_472
d_recoverOutcomeRef_472 ::
  Bool -> Bool -> T_RecoverOutcome'7580'_464
d_recoverOutcomeRef_472 v0 v1
  = if coe v0
      then if coe v1
             then coe C_recoveryBlocked'7580'_468
             else coe C_recoveryPosted'7580'_470
      else coe C_recoveryMissing'7580'_466
-- Hydra.Protocol.OffChainReference.inputAdmissionRef
hsInputAdmissionRef :: Integer -> Bool -> Bool -> Integer
hsInputAdmissionRef = coe d_inputAdmissionRef_474
d_inputAdmissionRef_474 :: Integer -> Bool -> Bool -> Integer
d_inputAdmissionRef_474 v0 v1 v2
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
hsClientDecommitRef = coe d_clientDecommitRef_476
d_clientDecommitRef_476 :: Bool -> Bool -> Bool -> Integer
d_clientDecommitRef_476 v0 v1 v2
  = if coe v0
      then coe (0 :: Integer)
      else (if coe v1
              then if coe v2 then coe (3 :: Integer) else coe (2 :: Integer)
              else coe (1 :: Integer))
-- Hydra.Protocol.OffChainReference.adoptSettlementRef
hsAdoptSettlementRef ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool -> Bool
hsAdoptSettlementRef = coe d_adoptSettlementRef_478
d_adoptSettlementRef_478 ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool -> Bool
d_adoptSettlementRef_478 v0 v1 v2 v3 v4 v5
  = coe
      d__'38''38'__34 (coe eqInt (coe v0) (coe v1))
      (coe
         d__'38''38'__34
         (coe eqInt (coe addInt (coe (1 :: Integer)) (coe v2)) (coe v3))
         (coe d__'38''38'__34 (coe v4) (coe v5)))
-- Hydra.Protocol.OffChainReference.repostFinalizedRef
hsRepostFinalizedRef :: Bool -> Integer -> Integer -> Bool
hsRepostFinalizedRef = coe d_repostFinalizedRef_492
d_repostFinalizedRef_492 :: Bool -> Integer -> Integer -> Bool
d_repostFinalizedRef_492 v0 v1 v2
  = coe d__'124''124'__50 (coe v0) (coe ltInt (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.fanoutContinuationRef
hsFanoutContinuationRef :: Bool -> Integer -> Integer
hsFanoutContinuationRef = coe d_fanoutContinuationRef_500
d_fanoutContinuationRef_500 :: Bool -> Integer -> Integer
d_fanoutContinuationRef_500 v0 v1
  = if coe v0
      then coe (1 :: Integer)
      else (let v2 = 0 :: Integer in
            coe
              (case coe v1 of
                 0 -> coe (1 :: Integer)
                 1 -> coe (2 :: Integer)
                 _ -> coe v2))
