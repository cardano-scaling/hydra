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
