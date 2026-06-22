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

module MAlonzo.Code.Hydra.Protocol.Reference where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text
import qualified MAlonzo.Code.Agda.Builtin.Bool
import qualified MAlonzo.Code.Agda.Builtin.List
import qualified MAlonzo.Code.Agda.Builtin.Nat

data HsCloseTag = CloseInitialT | CloseAnyT | CloseUnusedT | CloseUsedT
data HsOpen = MkOpen Integer Integer
data HsClosed = MkClosed Integer Integer Integer Integer Integer
data HsIncIO = MkIncIO Integer Integer Integer Integer Integer Integer Integer Integer
data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer
data HsFanout = MkFanout Integer Integer Integer Integer Integer
data HsRecoverIO = MkRecoverIO Integer Integer
data HsMintIO = MkMintIO Integer Integer
data HsClaimIO = MkClaimIO Integer Integer Integer Integer
data HsSignerIO = MkSignerIO [Integer] [Integer]
-- Hydra.Protocol.Reference.CloseTagᶜ
d_CloseTag'7580'_6 = ()
type T_CloseTag'7580'_6 = HsCloseTag
pattern C_closeInitial'7580'_8 = CloseInitialT
pattern C_closeAny'7580'_10 = CloseAnyT
pattern C_closeUnused'7580'_12 = CloseUnusedT
pattern C_closeUsed'7580'_14 = CloseUsedT
check_closeInitial'7580'_8 :: T_CloseTag'7580'_6
check_closeInitial'7580'_8 = CloseInitialT
check_closeAny'7580'_10 :: T_CloseTag'7580'_6
check_closeAny'7580'_10 = CloseAnyT
check_closeUnused'7580'_12 :: T_CloseTag'7580'_6
check_closeUnused'7580'_12 = CloseUnusedT
check_closeUsed'7580'_14 :: T_CloseTag'7580'_6
check_closeUsed'7580'_14 = CloseUsedT
cover_CloseTag'7580'_6 :: HsCloseTag -> ()
cover_CloseTag'7580'_6 x
  = case x of
      CloseInitialT -> ()
      CloseAnyT -> ()
      CloseUnusedT -> ()
      CloseUsedT -> ()
-- Hydra.Protocol.Reference.Openᶜ
d_Open'7580'_16 = ()
type T_Open'7580'_16 = HsOpen
pattern C_mkOpen'7580'_26 a0 a1 = MkOpen a0 a1
check_mkOpen'7580'_26 :: Integer -> Integer -> T_Open'7580'_16
check_mkOpen'7580'_26 = MkOpen
cover_Open'7580'_16 :: HsOpen -> ()
cover_Open'7580'_16 x
  = case x of
      MkOpen _ _ -> ()
-- Hydra.Protocol.Reference.Openᶜ.versionO
d_versionO_22 :: T_Open'7580'_16 -> Integer
d_versionO_22 v0
  = case coe v0 of
      C_mkOpen'7580'_26 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Openᶜ.cpO
d_cpO_24 :: T_Open'7580'_16 -> Integer
d_cpO_24 v0
  = case coe v0 of
      C_mkOpen'7580'_26 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ
d_Closed'7580'_28 = ()
type T_Closed'7580'_28 = HsClosed
pattern C_mkClosed'7580'_50 a0 a1 a2 a3 a4 = MkClosed a0 a1 a2 a3 a4
check_mkClosed'7580'_50 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_Closed'7580'_28
check_mkClosed'7580'_50 = MkClosed
cover_Closed'7580'_28 :: HsClosed -> ()
cover_Closed'7580'_28 x
  = case x of
      MkClosed _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.Closedᶜ.versionC
d_versionC_40 :: T_Closed'7580'_28 -> Integer
d_versionC_40 v0
  = case coe v0 of
      C_mkClosed'7580'_50 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.cpC
d_cpC_42 :: T_Closed'7580'_28 -> Integer
d_cpC_42 v0
  = case coe v0 of
      C_mkClosed'7580'_50 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.snapshotC
d_snapshotC_44 :: T_Closed'7580'_28 -> Integer
d_snapshotC_44 v0
  = case coe v0 of
      C_mkClosed'7580'_50 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.contesterLenC
d_contesterLenC_46 :: T_Closed'7580'_28 -> Integer
d_contesterLenC_46 v0
  = case coe v0 of
      C_mkClosed'7580'_50 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.tfinalC
d_tfinalC_48 :: T_Closed'7580'_28 -> Integer
d_tfinalC_48 v0
  = case coe v0 of
      C_mkClosed'7580'_50 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Ops
d_Ops_52 = ()
newtype T_Ops_52
  = C_Ops'46'constructor_125 (T_Open'7580'_16 ->
                              T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool)
-- Hydra.Protocol.Reference.Ops.closeCryptoOK
d_closeCryptoOK_56 ::
  T_Ops_52 ->
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
d_closeCryptoOK_56 v0
  = case coe v0 of
      C_Ops'46'constructor_125 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference._&&_
d__'38''38'__58 :: Bool -> Bool -> Bool
d__'38''38'__58 v0 v1 = if coe v0 then coe v1 else coe v0
-- Hydra.Protocol.Reference._||_
d__'124''124'__62 :: Bool -> Bool -> Bool
d__'124''124'__62 v0 v1 = if coe v0 then coe v0 else coe v1
-- Hydra.Protocol.Reference._==ᵇ_
d__'61''61''7495'__66 :: Integer -> Integer -> Bool
d__'61''61''7495'__66 v0 v1
  = case coe v0 of
      0 -> case coe v1 of
             0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             _ -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                     coe (coe d__'61''61''7495'__66 (coe v2) (coe v3)))
-- Hydra.Protocol.Reference._≤ᵇ_
d__'8804''7495'__72 :: Integer -> Integer -> Bool
d__'8804''7495'__72 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                     coe (coe d__'8804''7495'__72 (coe v2) (coe v3)))
-- Hydra.Protocol.Reference._<ᵇ_
d__'60''7495'__78 :: Integer -> Integer -> Bool
d__'60''7495'__78 v0 v1
  = coe
      d__'8804''7495'__72 (coe addInt (coe (1 :: Integer)) (coe v0))
      (coe v1)
-- Hydra.Protocol.Reference._≤ᴮ_
d__'8804''7470'__84 :: Integer -> Integer -> Bool
d__'8804''7470'__84 v0 v1
  = coe ltInt (coe v0) (coe addInt (coe (1 :: Integer)) (coe v1))
-- Hydra.Protocol.Reference.if_then_else_
d_if_then_else__92 :: () -> Bool -> AgdaAny -> AgdaAny -> AgdaAny
d_if_then_else__92 ~v0 v1 v2 v3 = du_if_then_else__92 v1 v2 v3
du_if_then_else__92 :: Bool -> AgdaAny -> AgdaAny -> AgdaAny
du_if_then_else__92 v0 v1 v2 = if coe v0 then coe v1 else coe v2
-- Hydra.Protocol.Reference.closeRefᵇ
d_closeRef'7495'_98 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> Integer -> Integer -> Bool
d_closeRef'7495'_98 v0 v1 v2 v3 v4 v5
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionO_22 (coe v1))
         (coe d_versionC_40 (coe v2)))
      (coe
         d__'38''38'__58
         (coe
            d__'61''61''7495'__66 (coe d_cpO_24 (coe v1))
            (coe d_cpC_42 (coe v2)))
         (coe
            d__'38''38'__58
            (coe
               d__'61''61''7495'__66 (coe d_contesterLenC_46 (coe v2))
               (coe (0 :: Integer)))
            (coe
               d__'38''38'__58 (coe du_initialOK_116 (coe v1) (coe v2) (coe v3))
               (coe
                  d__'38''38'__58 (coe du_anyOK_118 (coe v2) (coe v3))
                  (coe
                     d__'38''38'__58 (coe d_closeCryptoOK_56 v0 v1 v2 v3)
                     (coe
                        d__'38''38'__58
                        (coe
                           eqInt (coe d_tfinalC_48 (coe v2))
                           (coe addInt (coe d_cpO_24 (coe v1)) (coe v4)))
                        (coe
                           d__'8804''7470'__84
                           (coe MAlonzo.Code.Agda.Builtin.Nat.d__'45'__22 v4 v5)
                           (coe d_cpO_24 (coe v1)))))))))
-- Hydra.Protocol.Reference._.initialOK
d_initialOK_116 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 ->
  Integer -> Integer -> T_CloseTag'7580'_6 -> Bool
d_initialOK_116 ~v0 v1 v2 ~v3 ~v4 ~v5 v6
  = du_initialOK_116 v1 v2 v6
du_initialOK_116 ::
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_initialOK_116 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v2 of
         C_closeInitial'7580'_8
           -> coe
                d__'38''38'__58
                (coe
                   d__'61''61''7495'__66 (coe d_versionO_22 (coe v0))
                   (coe (0 :: Integer)))
                (coe
                   d__'61''61''7495'__66 (coe d_snapshotC_44 (coe v1))
                   (coe (0 :: Integer)))
         _ -> coe v3)
-- Hydra.Protocol.Reference._.anyOK
d_anyOK_118 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 ->
  Integer -> Integer -> T_CloseTag'7580'_6 -> Bool
d_anyOK_118 ~v0 ~v1 v2 ~v3 ~v4 ~v5 v6 = du_anyOK_118 v2 v6
du_anyOK_118 :: T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_anyOK_118 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v1 of
         C_closeAny'7580'_10
           -> coe
                d__'60''7495'__78 (coe (0 :: Integer))
                (coe d_snapshotC_44 (coe v0))
         _ -> coe v2)
-- Hydra.Protocol.Reference.IncIOᶜ
d_IncIO'7580'_120 = ()
type T_IncIO'7580'_120 = HsIncIO
pattern C_mkIncIO'7580'_154 a0 a1 a2 a3 a4 a5 a6 a7 = MkIncIO a0 a1 a2 a3 a4 a5 a6 a7
check_mkIncIO'7580'_154 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_IncIO'7580'_120
check_mkIncIO'7580'_154 = MkIncIO
cover_IncIO'7580'_120 :: HsIncIO -> ()
cover_IncIO'7580'_120 x
  = case x of
      MkIncIO _ _ _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.IncIOᶜ.versionIn
d_versionIn_138 :: T_IncIO'7580'_120 -> Integer
d_versionIn_138 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.versionOut
d_versionOut_140 :: T_IncIO'7580'_120 -> Integer
d_versionOut_140 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaIn
d_adaIn_142 :: T_IncIO'7580'_120 -> Integer
d_adaIn_142 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaDelta
d_adaDelta_144 :: T_IncIO'7580'_120 -> Integer
d_adaDelta_144 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaOut
d_adaOut_146 :: T_IncIO'7580'_120 -> Integer
d_adaOut_146 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.nonAdaIn
d_nonAdaIn_148 :: T_IncIO'7580'_120 -> Integer
d_nonAdaIn_148 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.nonAdaDelta
d_nonAdaDelta_150 :: T_IncIO'7580'_120 -> Integer
d_nonAdaDelta_150 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.nonAdaOut
d_nonAdaOut_152 :: T_IncIO'7580'_120 -> Integer
d_nonAdaOut_152 v0
  = case coe v0 of
      C_mkIncIO'7580'_154 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInc
d_OpsInc_156 = ()
newtype T_OpsInc_156
  = C_OpsInc'46'constructor_2927 (T_IncIO'7580'_120 -> Bool)
-- Hydra.Protocol.Reference.OpsInc.incCryptoOK
d_incCryptoOK_160 :: T_OpsInc_156 -> T_IncIO'7580'_120 -> Bool
d_incCryptoOK_160 v0
  = case coe v0 of
      C_OpsInc'46'constructor_2927 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.incRefᵇ
d_incRef'7495'_162 :: T_OpsInc_156 -> T_IncIO'7580'_120 -> Bool
d_incRef'7495'_162 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionOut_140 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_138 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            eqInt
            (coe
               addInt (coe d_adaDelta_144 (coe v1)) (coe d_adaIn_142 (coe v1)))
            (coe d_adaOut_146 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               eqInt
               (coe
                  addInt (coe d_nonAdaDelta_150 (coe v1))
                  (coe d_nonAdaIn_148 (coe v1)))
               (coe d_nonAdaOut_152 (coe v1)))
            (coe d_incCryptoOK_160 v0 v1)))
-- Hydra.Protocol.Reference.decRefᵇ
d_decRef'7495'_168 :: T_OpsInc_156 -> T_IncIO'7580'_120 -> Bool
d_decRef'7495'_168 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionOut_140 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_138 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            eqInt
            (coe
               addInt (coe d_adaOut_146 (coe v1)) (coe d_adaDelta_144 (coe v1)))
            (coe d_adaIn_142 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               eqInt
               (coe
                  addInt (coe d_nonAdaOut_152 (coe v1))
                  (coe d_nonAdaDelta_150 (coe v1)))
               (coe d_nonAdaIn_148 (coe v1)))
            (coe d_incCryptoOK_160 v0 v1)))
-- Hydra.Protocol.Reference.ContestIOᶜ
d_ContestIO'7580'_174 = ()
type T_ContestIO'7580'_174 = HsContestIO
pattern C_mkContestIO'7580'_220 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = MkContestIO a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
check_mkContestIO'7580'_220 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_ContestIO'7580'_174
check_mkContestIO'7580'_220 = MkContestIO
cover_ContestIO'7580'_174 :: HsContestIO -> ()
cover_ContestIO'7580'_174 x
  = case x of
      MkContestIO _ _ _ _ _ _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ContestIOᶜ.versionInK
d_versionInK_198 :: T_ContestIO'7580'_174 -> Integer
d_versionInK_198 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.versionOutK
d_versionOutK_200 :: T_ContestIO'7580'_174 -> Integer
d_versionOutK_200 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapIn
d_snapIn_202 :: T_ContestIO'7580'_174 -> Integer
d_snapIn_202 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapOut
d_snapOut_204 :: T_ContestIO'7580'_174 -> Integer
d_snapOut_204 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenIn
d_contesterLenIn_206 :: T_ContestIO'7580'_174 -> Integer
d_contesterLenIn_206 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenOut
d_contesterLenOut_208 :: T_ContestIO'7580'_174 -> Integer
d_contesterLenOut_208 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.tfinalK
d_tfinalK_210 :: T_ContestIO'7580'_174 -> Integer
d_tfinalK_210 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.validityHiK
d_validityHiK_212 :: T_ContestIO'7580'_174 -> Integer
d_validityHiK_212 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.tfinalOutK
d_tfinalOutK_214 :: T_ContestIO'7580'_174 -> Integer
d_tfinalOutK_214 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.numPartiesK
d_numPartiesK_216 :: T_ContestIO'7580'_174 -> Integer
d_numPartiesK_216 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.cpK
d_cpK_218 :: T_ContestIO'7580'_174 -> Integer
d_cpK_218 v0
  = case coe v0 of
      C_mkContestIO'7580'_220 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsContest
d_OpsContest_222 = ()
newtype T_OpsContest_222
  = C_OpsContest'46'constructor_3599 (T_ContestIO'7580'_174 -> Bool)
-- Hydra.Protocol.Reference.OpsContest.contestCryptoOK
d_contestCryptoOK_226 ::
  T_OpsContest_222 -> T_ContestIO'7580'_174 -> Bool
d_contestCryptoOK_226 v0
  = case coe v0 of
      C_OpsContest'46'constructor_3599 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.contestRefᵇ
d_contestRef'7495'_228 ::
  T_OpsContest_222 -> T_ContestIO'7580'_174 -> Bool
d_contestRef'7495'_228 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionInK_198 (coe v1))
         (coe d_versionOutK_200 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            d__'60''7495'__78 (coe d_snapIn_202 (coe v1))
            (coe d_snapOut_204 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               d__'61''61''7495'__66 (coe d_contesterLenOut_208 (coe v1))
               (coe
                  addInt (coe (1 :: Integer)) (coe d_contesterLenIn_206 (coe v1))))
            (coe
               d__'38''38'__58
               (coe
                  d__'8804''7470'__84 (coe d_validityHiK_212 (coe v1))
                  (coe d_tfinalK_210 (coe v1)))
               (coe
                  d__'38''38'__58
                  (coe
                     eqInt (coe d_tfinalOutK_214 (coe v1))
                     (coe
                        du_if_then_else__92
                        (coe
                           d__'61''61''7495'__66 (coe d_contesterLenOut_208 (coe v1))
                           (coe d_numPartiesK_216 (coe v1)))
                        (coe d_tfinalK_210 (coe v1))
                        (coe
                           addInt (coe d_cpK_218 (coe v1)) (coe d_tfinalK_210 (coe v1)))))
                  (coe d_contestCryptoOK_226 v0 v1)))))
-- Hydra.Protocol.Reference.Fanoutᶜ
d_Fanout'7580'_234 = ()
type T_Fanout'7580'_234 = HsFanout
pattern C_mkFanout'7580'_256 a0 a1 a2 a3 a4 = MkFanout a0 a1 a2 a3 a4
check_mkFanout'7580'_256 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_Fanout'7580'_234
check_mkFanout'7580'_256 = MkFanout
cover_Fanout'7580'_234 :: HsFanout -> ()
cover_Fanout'7580'_234 x
  = case x of
      MkFanout _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.Fanoutᶜ.numOutputsF
d_numOutputsF_246 :: T_Fanout'7580'_234 -> Integer
d_numOutputsF_246 v0
  = case coe v0 of
      C_mkFanout'7580'_256 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.burnedCountF
d_burnedCountF_248 :: T_Fanout'7580'_234 -> Integer
d_burnedCountF_248 v0
  = case coe v0 of
      C_mkFanout'7580'_256 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.numPartiesF
d_numPartiesF_250 :: T_Fanout'7580'_234 -> Integer
d_numPartiesF_250 v0
  = case coe v0 of
      C_mkFanout'7580'_256 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.tfinalF
d_tfinalF_252 :: T_Fanout'7580'_234 -> Integer
d_tfinalF_252 v0
  = case coe v0 of
      C_mkFanout'7580'_256 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.validityLoF
d_validityLoF_254 :: T_Fanout'7580'_234 -> Integer
d_validityLoF_254 v0
  = case coe v0 of
      C_mkFanout'7580'_256 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsFanout
d_OpsFanout_258 = ()
newtype T_OpsFanout_258
  = C_OpsFanout'46'constructor_3853 (T_Fanout'7580'_234 -> Bool)
-- Hydra.Protocol.Reference.OpsFanout.fanoutCryptoOK
d_fanoutCryptoOK_262 ::
  T_OpsFanout_258 -> T_Fanout'7580'_234 -> Bool
d_fanoutCryptoOK_262 v0
  = case coe v0 of
      C_OpsFanout'46'constructor_3853 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.fanoutRefᵇ
d_fanoutRef'7495'_264 ::
  T_OpsFanout_258 -> T_Fanout'7580'_234 -> Bool
d_fanoutRef'7495'_264 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'60''7495'__78 (coe (0 :: Integer))
         (coe d_numOutputsF_246 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            eqInt (coe d_burnedCountF_248 (coe v1))
            (coe addInt (coe (1 :: Integer)) (coe d_numPartiesF_250 (coe v1))))
         (coe
            d__'38''38'__58
            (coe
               ltInt (coe d_tfinalF_252 (coe v1))
               (coe d_validityLoF_254 (coe v1)))
            (coe d_fanoutCryptoOK_262 v0 v1)))
-- Hydra.Protocol.Reference.RecoverIOᶜ
d_RecoverIO'7580'_270 = ()
type T_RecoverIO'7580'_270 = HsRecoverIO
pattern C_mkRecoverIO'7580'_280 a0 a1 = MkRecoverIO a0 a1
check_mkRecoverIO'7580'_280 ::
  Integer -> Integer -> T_RecoverIO'7580'_270
check_mkRecoverIO'7580'_280 = MkRecoverIO
cover_RecoverIO'7580'_270 :: HsRecoverIO -> ()
cover_RecoverIO'7580'_270 x
  = case x of
      MkRecoverIO _ _ -> ()
-- Hydra.Protocol.Reference.RecoverIOᶜ.tRecoverR
d_tRecoverR_276 :: T_RecoverIO'7580'_270 -> Integer
d_tRecoverR_276 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_280 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.RecoverIOᶜ.validityLoR
d_validityLoR_278 :: T_RecoverIO'7580'_270 -> Integer
d_validityLoR_278 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_280 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsRecover
d_OpsRecover_282 = ()
newtype T_OpsRecover_282
  = C_OpsRecover'46'constructor_3957 (T_RecoverIO'7580'_270 -> Bool)
-- Hydra.Protocol.Reference.OpsRecover.recoverHashOK
d_recoverHashOK_286 ::
  T_OpsRecover_282 -> T_RecoverIO'7580'_270 -> Bool
d_recoverHashOK_286 v0
  = case coe v0 of
      C_OpsRecover'46'constructor_3957 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.recoverRefᵇ
d_recoverRef'7495'_288 ::
  T_OpsRecover_282 -> T_RecoverIO'7580'_270 -> Bool
d_recoverRef'7495'_288 v0 v1
  = coe
      d__'38''38'__58
      (coe
         ltInt (coe d_tRecoverR_276 (coe v1))
         (coe d_validityLoR_278 (coe v1)))
      (coe d_recoverHashOK_286 v0 v1)
-- Hydra.Protocol.Reference.MintIOᶜ
d_MintIO'7580'_294 = ()
type T_MintIO'7580'_294 = HsMintIO
pattern C_mkMintIO'7580'_304 a0 a1 = MkMintIO a0 a1
check_mkMintIO'7580'_304 ::
  Integer -> Integer -> T_MintIO'7580'_294
check_mkMintIO'7580'_304 = MkMintIO
cover_MintIO'7580'_294 :: HsMintIO -> ()
cover_MintIO'7580'_294 x
  = case x of
      MkMintIO _ _ -> ()
-- Hydra.Protocol.Reference.MintIOᶜ.numPartiesM
d_numPartiesM_300 :: T_MintIO'7580'_294 -> Integer
d_numPartiesM_300 v0
  = case coe v0 of
      C_mkMintIO'7580'_304 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.MintIOᶜ.mintedCountM
d_mintedCountM_302 :: T_MintIO'7580'_294 -> Integer
d_mintedCountM_302 v0
  = case coe v0 of
      C_mkMintIO'7580'_304 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInit
d_OpsInit_306 = ()
newtype T_OpsInit_306
  = C_OpsInit'46'constructor_4037 (T_MintIO'7580'_294 -> Bool)
-- Hydra.Protocol.Reference.OpsInit.initPlacementOK
d_initPlacementOK_310 ::
  T_OpsInit_306 -> T_MintIO'7580'_294 -> Bool
d_initPlacementOK_310 v0
  = case coe v0 of
      C_OpsInit'46'constructor_4037 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.initRefᵇ
d_initRef'7495'_312 :: T_OpsInit_306 -> T_MintIO'7580'_294 -> Bool
d_initRef'7495'_312 v0 v1
  = coe
      d__'38''38'__58
      (coe
         eqInt (coe d_mintedCountM_302 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_numPartiesM_300 (coe v1))))
      (coe d_initPlacementOK_310 v0 v1)
-- Hydra.Protocol.Reference.ClaimIOᶜ
d_ClaimIO'7580'_318 = ()
type T_ClaimIO'7580'_318 = HsClaimIO
pattern C_mkClaimIO'7580'_336 a0 a1 a2 a3 = MkClaimIO a0 a1 a2 a3
check_mkClaimIO'7580'_336 ::
  Integer -> Integer -> Integer -> Integer -> T_ClaimIO'7580'_318
check_mkClaimIO'7580'_336 = MkClaimIO
cover_ClaimIO'7580'_318 :: HsClaimIO -> ()
cover_ClaimIO'7580'_318 x
  = case x of
      MkClaimIO _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ClaimIOᶜ.tRecoverC
d_tRecoverC_328 :: T_ClaimIO'7580'_318 -> Integer
d_tRecoverC_328 v0
  = case coe v0 of
      C_mkClaimIO'7580'_336 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.validityHiC
d_validityHiC_330 :: T_ClaimIO'7580'_318 -> Integer
d_validityHiC_330 v0
  = case coe v0 of
      C_mkClaimIO'7580'_336 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.depositCidC
d_depositCidC_332 :: T_ClaimIO'7580'_318 -> Integer
d_depositCidC_332 v0
  = case coe v0 of
      C_mkClaimIO'7580'_336 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.headCidC
d_headCidC_334 :: T_ClaimIO'7580'_318 -> Integer
d_headCidC_334 v0
  = case coe v0 of
      C_mkClaimIO'7580'_336 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsClaim
d_OpsClaim_338 = ()
newtype T_OpsClaim_338
  = C_OpsClaim'46'constructor_4167 (T_ClaimIO'7580'_318 -> Bool)
-- Hydra.Protocol.Reference.OpsClaim.claimIncrementOK
d_claimIncrementOK_342 ::
  T_OpsClaim_338 -> T_ClaimIO'7580'_318 -> Bool
d_claimIncrementOK_342 v0
  = case coe v0 of
      C_OpsClaim'46'constructor_4167 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.claimRefᵇ
d_claimRef'7495'_344 ::
  T_OpsClaim_338 -> T_ClaimIO'7580'_318 -> Bool
d_claimRef'7495'_344 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'8804''7470'__84 (coe d_validityHiC_330 (coe v1))
         (coe d_tRecoverC_328 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            eqInt (coe d_depositCidC_332 (coe v1))
            (coe d_headCidC_334 (coe v1)))
         (coe d_claimIncrementOK_342 v0 v1))
-- Hydra.Protocol.Reference.elemᵇ
d_elem'7495'_350 :: Integer -> [Integer] -> Bool
d_elem'7495'_350 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__62 (coe eqInt (coe v0) (coe v2))
             (coe d_elem'7495'_350 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.anySharedᵇ
d_anyShared'7495'_358 :: [Integer] -> [Integer] -> Bool
d_anyShared'7495'_358 v0 v1
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__62 (coe d_elem'7495'_350 (coe v2) (coe v1))
             (coe d_anyShared'7495'_358 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.SignerIOᶜ
d_SignerIO'7580'_366 = ()
type T_SignerIO'7580'_366 = HsSignerIO
pattern C_mkSignerIO'7580'_376 a0 a1 = MkSignerIO a0 a1
check_mkSignerIO'7580'_376 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  T_SignerIO'7580'_366
check_mkSignerIO'7580'_376 = MkSignerIO
cover_SignerIO'7580'_366 :: HsSignerIO -> ()
cover_SignerIO'7580'_366 x
  = case x of
      MkSignerIO _ _ -> ()
-- Hydra.Protocol.Reference.SignerIOᶜ.signerCodesS
d_signerCodesS_372 :: T_SignerIO'7580'_366 -> [Integer]
d_signerCodesS_372 v0
  = case coe v0 of
      C_mkSignerIO'7580'_376 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.SignerIOᶜ.ptCodesS
d_ptCodesS_374 :: T_SignerIO'7580'_366 -> [Integer]
d_ptCodesS_374 v0
  = case coe v0 of
      C_mkSignerIO'7580'_376 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.participantSignedRefᵇ
d_participantSignedRef'7495'_378 :: T_SignerIO'7580'_366 -> Bool
d_participantSignedRef'7495'_378 v0
  = coe
      d_anyShared'7495'_358 (coe d_signerCodesS_372 (coe v0))
      (coe d_ptCodesS_374 (coe v0))