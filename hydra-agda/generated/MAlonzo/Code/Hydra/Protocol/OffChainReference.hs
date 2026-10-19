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
  Integer ->
  Integer -> Integer -> Bool -> Integer -> Integer -> Bool -> Bool
hsSignEligibleRef = coe d_signEligibleRef_38
d_signEligibleRef_38 ::
  Integer ->
  Integer -> Integer -> Bool -> Integer -> Integer -> Bool -> Bool
d_signEligibleRef_38 v0 v1 v2 v3 v4 v5 v6
  = coe
      d__'38''38'__34
      (coe du_versionOk_58 (coe v0) (coe v1) (coe v2) (coe v3))
      (coe
         d__'38''38'__34
         (coe eqInt (coe v4) (coe addInt (coe (1 :: Integer)) (coe v5)))
         (coe v6))
-- Hydra.Protocol.OffChainReference._.versionOk
d_versionOk_58 ::
  Integer ->
  Integer -> Integer -> Bool -> Integer -> Integer -> Bool -> Bool
d_versionOk_58 v0 v1 v2 v3 ~v4 ~v5 ~v6
  = du_versionOk_58 v0 v1 v2 v3
du_versionOk_58 :: Integer -> Integer -> Integer -> Bool -> Bool
du_versionOk_58 v0 v1 v2 v3
  = coe
      du_if_then_else__16 (coe eqInt (coe v0) (coe v1))
      (coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10)
      (coe
         d__'38''38'__34
         (coe eqInt (coe addInt (coe (1 :: Integer)) (coe v0)) (coe v1))
         (coe d__'38''38'__34 (coe eqInt (coe v0) (coe v2)) (coe v3)))
-- Hydra.Protocol.OffChainReference._||_
d__'124''124'__60 :: Bool -> Bool -> Bool
d__'124''124'__60 v0 v1 = if coe v0 then coe v0 else coe v1
-- Hydra.Protocol.OffChainReference.not
d_not_64 :: Bool -> Bool
d_not_64 v0
  = if coe v0
      then coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      else coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
-- Hydra.Protocol.OffChainReference.elemᵇ
d_elem'7495'_66 :: Integer -> [Integer] -> Bool
d_elem'7495'_66 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__60 (coe eqInt (coe v0) (coe v2))
             (coe d_elem'7495'_66 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.PendingCommitᶜ
d_PendingCommit'7580'_74 = ()
type T_PendingCommit'7580'_74 = HsPendingCommit
pattern C_noCommit'7580'_76 = NoCommitP
pattern C_commitPending'7580'_78 = CommitPendingP
pattern C_commitExpired'7580'_80 = CommitExpiredP
pattern C_commitGone'7580'_82 = CommitGoneP
check_noCommit'7580'_76 :: T_PendingCommit'7580'_74
check_noCommit'7580'_76 = NoCommitP
check_commitPending'7580'_78 :: T_PendingCommit'7580'_74
check_commitPending'7580'_78 = CommitPendingP
check_commitExpired'7580'_80 :: T_PendingCommit'7580'_74
check_commitExpired'7580'_80 = CommitExpiredP
check_commitGone'7580'_82 :: T_PendingCommit'7580'_74
check_commitGone'7580'_82 = CommitGoneP
cover_PendingCommit'7580'_74 :: HsPendingCommit -> ()
cover_PendingCommit'7580'_74 x
  = case x of
      NoCommitP -> ()
      CommitPendingP -> ()
      CommitExpiredP -> ()
      CommitGoneP -> ()
-- Hydra.Protocol.OffChainReference.commitBlocksᵇ
d_commitBlocks'7495'_84 :: T_PendingCommit'7580'_74 -> Bool
d_commitBlocks'7495'_84 v0
  = case coe v0 of
      C_noCommit'7580'_76 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      C_commitPending'7580'_78
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      C_commitExpired'7580'_80
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      C_commitGone'7580'_82
        -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.reqDecEligibleRef
hsReqDecEligibleRef :: T_PendingCommit'7580'_74 -> Bool -> Bool
hsReqDecEligibleRef = coe d_reqDecEligibleRef_86
d_reqDecEligibleRef_86 :: T_PendingCommit'7580'_74 -> Bool -> Bool
d_reqDecEligibleRef_86 v0 v1
  = coe
      d__'38''38'__34
      (coe d_not_64 (coe d_commitBlocks'7495'_84 (coe v0)))
      (coe d_not_64 (coe v1))
-- Hydra.Protocol.OffChainReference.reqSnNotBothRef
hsReqSnNotBothRef :: Bool -> Bool -> Bool
hsReqSnNotBothRef = coe d_reqSnNotBothRef_92
d_reqSnNotBothRef_92 :: Bool -> Bool -> Bool
d_reqSnNotBothRef_92 v0 v1
  = coe d_not_64 (coe d__'38''38'__34 (coe v0) (coe v1))
-- Hydra.Protocol.OffChainReference.reqSnDecommitOutputsRef
hsReqSnDecommitOutputsRef :: Integer -> Bool
hsReqSnDecommitOutputsRef = coe d_reqSnDecommitOutputsRef_98
d_reqSnDecommitOutputsRef_98 :: Integer -> Bool
d_reqSnDecommitOutputsRef_98 v0
  = coe ltInt (coe (0 :: Integer)) (coe v0)
-- Hydra.Protocol.OffChainReference.reqSnDepositSettledRef
hsReqSnDepositSettledRef :: Bool -> Integer -> Integer -> Bool
hsReqSnDepositSettledRef = coe d_reqSnDepositSettledRef_102
d_reqSnDepositSettledRef_102 :: Bool -> Integer -> Integer -> Bool
d_reqSnDepositSettledRef_102 v0 v1 v2
  = coe d__'38''38'__34 (coe v0) (coe eqInt (coe v1) (coe v2))
-- Hydra.Protocol.OffChainReference.reqSnDecommitSettledRef
hsReqSnDecommitSettledRef :: Bool -> Bool -> Bool
hsReqSnDecommitSettledRef = coe d_reqSnDecommitSettledRef_110
d_reqSnDecommitSettledRef_110 :: Bool -> Bool -> Bool
d_reqSnDecommitSettledRef_110 v0 v1
  = coe d__'38''38'__34 (coe v0) (coe v1)
-- Hydra.Protocol.OffChainReference.disjointᵇ
d_disjoint'7495'_116 :: [Integer] -> [Integer] -> Bool
d_disjoint'7495'_116 v0 v1
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v2 v3
        -> coe
             d__'38''38'__34
             (coe d_not_64 (coe d_elem'7495'_66 (coe v2) (coe v1)))
             (coe d_disjoint'7495'_116 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.reqSnDepositDisjointRef
hsReqSnDepositDisjointRef ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsReqSnDepositDisjointRef = coe d_reqSnDepositDisjointRef_124
d_reqSnDepositDisjointRef_124 ::
  [Integer] -> [Integer] -> [Integer] -> Bool
d_reqSnDepositDisjointRef_124 v0 v1 v2
  = coe
      d__'38''38'__34 (coe d_disjoint'7495'_116 (coe v2) (coe v0))
      (coe d_disjoint'7495'_116 (coe v2) (coe v1))
-- Hydra.Protocol.OffChainReference.notAlreadySignedRef
hsNotAlreadySignedRef ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  Integer -> Bool
hsNotAlreadySignedRef = coe d_notAlreadySignedRef_132
d_notAlreadySignedRef_132 :: [Integer] -> Integer -> Bool
d_notAlreadySignedRef_132 v0 v1
  = coe d_not_64 (coe d_elem'7495'_66 (coe v1) (coe v0))
-- Hydra.Protocol.OffChainReference.allBelowᵇ
d_allBelow'7495'_138 :: Integer -> [Integer] -> Bool
d_allBelow'7495'_138 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (coe
                d__'38''38'__34 (coe d_elem'7495'_66 (coe v2) (coe v1))
                (coe d_allBelow'7495'_138 (coe v2) (coe v1)))
-- Hydra.Protocol.OffChainReference.allSignedRef
hsAllSignedRef ::
  Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer -> Bool
hsAllSignedRef = coe d_allSignedRef_144
d_allSignedRef_144 :: Integer -> [Integer] -> Bool
d_allSignedRef_144 v0 v1
  = coe d_allBelow'7495'_138 (coe v0) (coe v1)
-- Hydra.Protocol.OffChainReference.contestEligibleRef
hsContestEligibleRef :: Integer -> Integer -> Bool
hsContestEligibleRef = coe d_contestEligibleRef_150
d_contestEligibleRef_150 :: Integer -> Integer -> Bool
d_contestEligibleRef_150 v0 v1 = coe ltInt (coe v1) (coe v0)
-- Hydra.Protocol.OffChainReference.modSuc
d_modSuc_156 :: Integer -> Integer -> Integer
d_modSuc_156 v0 v1
  = coe remInt (coe v0) (coe addInt (coe (1 :: Integer)) (coe v1))
-- Hydra.Protocol.OffChainReference.leaderRef
hsLeaderRef :: Integer -> Integer -> Integer -> Bool
hsLeaderRef = coe d_leaderRef_162
d_leaderRef_162 :: Integer -> Integer -> Integer -> Bool
d_leaderRef_162 v0 v1 v2
  = coe
      eqInt (coe d_modSuc_156 (coe addInt (coe v0) (coe v1)) (coe v0))
      (coe v2)
