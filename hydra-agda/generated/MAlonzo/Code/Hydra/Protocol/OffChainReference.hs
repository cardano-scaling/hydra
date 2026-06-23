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
import qualified MAlonzo.Code.Agda.Builtin.Nat

data HsDepositStatus = InactiveS | ActiveS | ExpiredS deriving (Eq, Show)
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
d_depositStatusRef_22 ::
  Integer -> Integer -> Integer -> Integer -> T_DepositStatus'7580'_6
d_depositStatusRef_22 v0 v1 v2 v3
  = coe
      du_if_then_else__16
      (coe
         ltInt (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1 v2)
         (coe v3))
      (coe C_expired'7580'_12)
      (coe
         du_if_then_else__16
         (coe ltInt (coe addInt (coe v0) (coe v2)) (coe v3))
         (coe C_active'7580'_10) (coe C_inactive'7580'_8))
-- Hydra.Protocol.OffChainReference._&&_
d__'38''38'__32 :: Bool -> Bool -> Bool
d__'38''38'__32 v0 v1 = if coe v0 then coe v1 else coe v0
-- Hydra.Protocol.OffChainReference.signEligibleRef
d_signEligibleRef_36 ::
  Integer -> Integer -> Integer -> Integer -> Bool -> Bool
d_signEligibleRef_36 v0 v1 v2 v3 v4
  = coe
      d__'38''38'__32 (coe eqInt (coe v0) (coe v1))
      (coe
         d__'38''38'__32
         (coe eqInt (coe v2) (coe addInt (coe (1 :: Integer)) (coe v3)))
         (coe v4))
-- Hydra.Protocol.OffChainReference._||_
d__'124''124'__48 :: Bool -> Bool -> Bool
d__'124''124'__48 v0 v1 = if coe v0 then coe v0 else coe v1
-- Hydra.Protocol.OffChainReference.not
d_not_52 :: Bool -> Bool
d_not_52 v0
  = if coe v0
      then coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      else coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
-- Hydra.Protocol.OffChainReference.elemᵇ
d_elem'7495'_54 :: Integer -> [Integer] -> Bool
d_elem'7495'_54 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__48 (coe eqInt (coe v0) (coe v2))
             (coe d_elem'7495'_54 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.OffChainReference.reqDecEligibleRef
d_reqDecEligibleRef_62 :: Bool -> Bool -> Bool
d_reqDecEligibleRef_62 v0 v1
  = coe
      d__'38''38'__32 (coe d_not_52 (coe v0)) (coe d_not_52 (coe v1))
-- Hydra.Protocol.OffChainReference.notAlreadySignedRef
d_notAlreadySignedRef_68 :: [Integer] -> Integer -> Bool
d_notAlreadySignedRef_68 v0 v1
  = coe d_not_52 (coe d_elem'7495'_54 (coe v1) (coe v0))
-- Hydra.Protocol.OffChainReference.allBelowᵇ
d_allBelow'7495'_74 :: Integer -> [Integer] -> Bool
d_allBelow'7495'_74 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (coe
                d__'38''38'__32 (coe d_elem'7495'_54 (coe v2) (coe v1))
                (coe d_allBelow'7495'_74 (coe v2) (coe v1)))
-- Hydra.Protocol.OffChainReference.allSignedRef
d_allSignedRef_80 :: Integer -> [Integer] -> Bool
d_allSignedRef_80 v0 v1 = coe d_allBelow'7495'_74 (coe v0) (coe v1)
-- Hydra.Protocol.OffChainReference.contestEligibleRef
d_contestEligibleRef_86 :: Integer -> Integer -> Bool
d_contestEligibleRef_86 v0 v1 = coe ltInt (coe v1) (coe v0)
-- Hydra.Protocol.OffChainReference.modSuc
d_modSuc_92 :: Integer -> Integer -> Integer
d_modSuc_92 v0 v1
  = coe remInt (coe v0) (coe addInt (coe (1 :: Integer)) (coe v1))
-- Hydra.Protocol.OffChainReference.leaderRef
d_leaderRef_98 :: Integer -> Integer -> Integer -> Bool
d_leaderRef_98 v0 v1 v2
  = coe
      eqInt
      (coe
         d_modSuc_92
         (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v1 (1 :: Integer))
         (coe v0))
      (coe v2)