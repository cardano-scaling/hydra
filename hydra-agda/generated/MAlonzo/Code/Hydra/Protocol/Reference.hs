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
data HsIncIO = MkIncIO Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer
data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer
data HsFanout = MkFanout Integer Integer Integer Integer Integer
data HsRecoverIO = MkRecoverIO Integer Integer Integer
data HsMintIO = MkMintIO Integer Integer Integer Integer
data HsClaimIO = MkClaimIO Integer Integer Integer Integer Integer Integer Integer
data HsSignerIO = MkSignerIO [Integer] [Integer]
data HsAssetIO = MkAssetIO Integer Integer Integer
data HsBurnIO = MkBurnIO Integer Integer
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
         (coe eqInt (coe d_cpO_24 (coe v1)) (coe d_cpC_42 (coe v2)))
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
pattern C_mkIncIO'7580'_162 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 = MkIncIO a0 a1 a2 a3 a4 a5 a6 a7 a8 a9
check_mkIncIO'7580'_162 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_IncIO'7580'_120
check_mkIncIO'7580'_162 = MkIncIO
cover_IncIO'7580'_120 :: HsIncIO -> ()
cover_IncIO'7580'_120 x
  = case x of
      MkIncIO _ _ _ _ _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.IncIOᶜ.versionIn
d_versionIn_142 :: T_IncIO'7580'_120 -> Integer
d_versionIn_142 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.versionOut
d_versionOut_144 :: T_IncIO'7580'_120 -> Integer
d_versionOut_144 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaIn
d_adaIn_146 :: T_IncIO'7580'_120 -> Integer
d_adaIn_146 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaDelta
d_adaDelta_148 :: T_IncIO'7580'_120 -> Integer
d_adaDelta_148 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaOut
d_adaOut_150 :: T_IncIO'7580'_120 -> Integer
d_adaOut_150 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.nonAdaIn
d_nonAdaIn_152 :: T_IncIO'7580'_120 -> Integer
d_nonAdaIn_152 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.nonAdaDelta
d_nonAdaDelta_154 :: T_IncIO'7580'_120 -> Integer
d_nonAdaDelta_154 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.nonAdaOut
d_nonAdaOut_156 :: T_IncIO'7580'_120 -> Integer
d_nonAdaOut_156 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.depositIdxI
d_depositIdxI_158 :: T_IncIO'7580'_120 -> Integer
d_depositIdxI_158 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.numDecOutsI
d_numDecOutsI_160 :: T_IncIO'7580'_120 -> Integer
d_numDecOutsI_160 v0
  = case coe v0 of
      C_mkIncIO'7580'_162 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInc
d_OpsInc_164 = ()
newtype T_OpsInc_164
  = C_OpsInc'46'constructor_3071 (T_IncIO'7580'_120 -> Bool)
-- Hydra.Protocol.Reference.OpsInc.incCryptoOK
d_incCryptoOK_168 :: T_OpsInc_164 -> T_IncIO'7580'_120 -> Bool
d_incCryptoOK_168 v0
  = case coe v0 of
      C_OpsInc'46'constructor_3071 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.incRefᵇ
d_incRef'7495'_170 :: T_OpsInc_164 -> T_IncIO'7580'_120 -> Bool
d_incRef'7495'_170 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionOut_144 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_142 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            d__'61''61''7495'__66 (coe d_depositIdxI_158 (coe v1))
            (coe (0 :: Integer)))
         (coe
            d__'38''38'__58
            (coe
               eqInt
               (coe
                  addInt (coe d_adaDelta_148 (coe v1)) (coe d_adaIn_146 (coe v1)))
               (coe d_adaOut_150 (coe v1)))
            (coe
               d__'38''38'__58
               (coe
                  eqInt
                  (coe
                     addInt (coe d_nonAdaDelta_154 (coe v1))
                     (coe d_nonAdaIn_152 (coe v1)))
                  (coe d_nonAdaOut_156 (coe v1)))
               (coe d_incCryptoOK_168 v0 v1))))
-- Hydra.Protocol.Reference.decRefᵇ
d_decRef'7495'_176 :: T_OpsInc_164 -> T_IncIO'7580'_120 -> Bool
d_decRef'7495'_176 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionOut_144 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_142 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            d__'60''7495'__78 (coe (0 :: Integer))
            (coe d_numDecOutsI_160 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               eqInt
               (coe
                  addInt (coe d_adaOut_150 (coe v1)) (coe d_adaDelta_148 (coe v1)))
               (coe d_adaIn_146 (coe v1)))
            (coe
               d__'38''38'__58
               (coe
                  eqInt
                  (coe
                     addInt (coe d_nonAdaOut_156 (coe v1))
                     (coe d_nonAdaDelta_154 (coe v1)))
                  (coe d_nonAdaIn_152 (coe v1)))
               (coe d_incCryptoOK_168 v0 v1))))
-- Hydra.Protocol.Reference.ContestIOᶜ
d_ContestIO'7580'_182 = ()
type T_ContestIO'7580'_182 = HsContestIO
pattern C_mkContestIO'7580'_228 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = MkContestIO a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
check_mkContestIO'7580'_228 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_ContestIO'7580'_182
check_mkContestIO'7580'_228 = MkContestIO
cover_ContestIO'7580'_182 :: HsContestIO -> ()
cover_ContestIO'7580'_182 x
  = case x of
      MkContestIO _ _ _ _ _ _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ContestIOᶜ.versionInK
d_versionInK_206 :: T_ContestIO'7580'_182 -> Integer
d_versionInK_206 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.versionOutK
d_versionOutK_208 :: T_ContestIO'7580'_182 -> Integer
d_versionOutK_208 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapIn
d_snapIn_210 :: T_ContestIO'7580'_182 -> Integer
d_snapIn_210 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapOut
d_snapOut_212 :: T_ContestIO'7580'_182 -> Integer
d_snapOut_212 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenIn
d_contesterLenIn_214 :: T_ContestIO'7580'_182 -> Integer
d_contesterLenIn_214 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenOut
d_contesterLenOut_216 :: T_ContestIO'7580'_182 -> Integer
d_contesterLenOut_216 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.tfinalK
d_tfinalK_218 :: T_ContestIO'7580'_182 -> Integer
d_tfinalK_218 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.validityHiK
d_validityHiK_220 :: T_ContestIO'7580'_182 -> Integer
d_validityHiK_220 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.tfinalOutK
d_tfinalOutK_222 :: T_ContestIO'7580'_182 -> Integer
d_tfinalOutK_222 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v9
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.numPartiesK
d_numPartiesK_224 :: T_ContestIO'7580'_182 -> Integer
d_numPartiesK_224 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v10
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.cpK
d_cpK_226 :: T_ContestIO'7580'_182 -> Integer
d_cpK_226 v0
  = case coe v0 of
      C_mkContestIO'7580'_228 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
        -> coe v11
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsContest
d_OpsContest_230 = ()
newtype T_OpsContest_230
  = C_OpsContest'46'constructor_3763 (T_ContestIO'7580'_182 -> Bool)
-- Hydra.Protocol.Reference.OpsContest.contestCryptoOK
d_contestCryptoOK_234 ::
  T_OpsContest_230 -> T_ContestIO'7580'_182 -> Bool
d_contestCryptoOK_234 v0
  = case coe v0 of
      C_OpsContest'46'constructor_3763 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.contestRefᵇ
d_contestRef'7495'_236 ::
  T_OpsContest_230 -> T_ContestIO'7580'_182 -> Bool
d_contestRef'7495'_236 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__66 (coe d_versionInK_206 (coe v1))
         (coe d_versionOutK_208 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            d__'60''7495'__78 (coe d_snapIn_210 (coe v1))
            (coe d_snapOut_212 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               d__'61''61''7495'__66 (coe d_contesterLenOut_216 (coe v1))
               (coe
                  addInt (coe (1 :: Integer)) (coe d_contesterLenIn_214 (coe v1))))
            (coe
               d__'38''38'__58
               (coe
                  d__'8804''7470'__84 (coe d_validityHiK_220 (coe v1))
                  (coe d_tfinalK_218 (coe v1)))
               (coe
                  d__'38''38'__58
                  (coe
                     eqInt (coe d_tfinalOutK_222 (coe v1))
                     (coe
                        du_if_then_else__92
                        (coe
                           d__'61''61''7495'__66 (coe d_contesterLenOut_216 (coe v1))
                           (coe d_numPartiesK_224 (coe v1)))
                        (coe d_tfinalK_218 (coe v1))
                        (coe
                           addInt (coe d_cpK_226 (coe v1)) (coe d_tfinalK_218 (coe v1)))))
                  (coe d_contestCryptoOK_234 v0 v1)))))
-- Hydra.Protocol.Reference.Fanoutᶜ
d_Fanout'7580'_242 = ()
type T_Fanout'7580'_242 = HsFanout
pattern C_mkFanout'7580'_264 a0 a1 a2 a3 a4 = MkFanout a0 a1 a2 a3 a4
check_mkFanout'7580'_264 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_Fanout'7580'_242
check_mkFanout'7580'_264 = MkFanout
cover_Fanout'7580'_242 :: HsFanout -> ()
cover_Fanout'7580'_242 x
  = case x of
      MkFanout _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.Fanoutᶜ.numOutputsF
d_numOutputsF_254 :: T_Fanout'7580'_242 -> Integer
d_numOutputsF_254 v0
  = case coe v0 of
      C_mkFanout'7580'_264 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.burnedCountF
d_burnedCountF_256 :: T_Fanout'7580'_242 -> Integer
d_burnedCountF_256 v0
  = case coe v0 of
      C_mkFanout'7580'_264 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.numPartiesF
d_numPartiesF_258 :: T_Fanout'7580'_242 -> Integer
d_numPartiesF_258 v0
  = case coe v0 of
      C_mkFanout'7580'_264 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.tfinalF
d_tfinalF_260 :: T_Fanout'7580'_242 -> Integer
d_tfinalF_260 v0
  = case coe v0 of
      C_mkFanout'7580'_264 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.validityLoF
d_validityLoF_262 :: T_Fanout'7580'_242 -> Integer
d_validityLoF_262 v0
  = case coe v0 of
      C_mkFanout'7580'_264 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsFanout
d_OpsFanout_266 = ()
newtype T_OpsFanout_266
  = C_OpsFanout'46'constructor_4017 (T_Fanout'7580'_242 -> Bool)
-- Hydra.Protocol.Reference.OpsFanout.fanoutCryptoOK
d_fanoutCryptoOK_270 ::
  T_OpsFanout_266 -> T_Fanout'7580'_242 -> Bool
d_fanoutCryptoOK_270 v0
  = case coe v0 of
      C_OpsFanout'46'constructor_4017 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.fanoutRefᵇ
d_fanoutRef'7495'_272 ::
  T_OpsFanout_266 -> T_Fanout'7580'_242 -> Bool
d_fanoutRef'7495'_272 v0 v1
  = coe
      d__'38''38'__58
      (coe
         eqInt (coe d_burnedCountF_256 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_numPartiesF_258 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            ltInt (coe d_tfinalF_260 (coe v1))
            (coe d_validityLoF_262 (coe v1)))
         (coe d_fanoutCryptoOK_270 v0 v1))
-- Hydra.Protocol.Reference.RecoverIOᶜ
d_RecoverIO'7580'_278 = ()
type T_RecoverIO'7580'_278 = HsRecoverIO
pattern C_mkRecoverIO'7580'_292 a0 a1 a2 = MkRecoverIO a0 a1 a2
check_mkRecoverIO'7580'_292 ::
  Integer -> Integer -> Integer -> T_RecoverIO'7580'_278
check_mkRecoverIO'7580'_292 = MkRecoverIO
cover_RecoverIO'7580'_278 :: HsRecoverIO -> ()
cover_RecoverIO'7580'_278 x
  = case x of
      MkRecoverIO _ _ _ -> ()
-- Hydra.Protocol.Reference.RecoverIOᶜ.tRecoverR
d_tRecoverR_286 :: T_RecoverIO'7580'_278 -> Integer
d_tRecoverR_286 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_292 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.RecoverIOᶜ.validityLoR
d_validityLoR_288 :: T_RecoverIO'7580'_278 -> Integer
d_validityLoR_288 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_292 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.RecoverIOᶜ.depositCountR
d_depositCountR_290 :: T_RecoverIO'7580'_278 -> Integer
d_depositCountR_290 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_292 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsRecover
d_OpsRecover_294 = ()
newtype T_OpsRecover_294
  = C_OpsRecover'46'constructor_4131 (T_RecoverIO'7580'_278 -> Bool)
-- Hydra.Protocol.Reference.OpsRecover.recoverHashOK
d_recoverHashOK_298 ::
  T_OpsRecover_294 -> T_RecoverIO'7580'_278 -> Bool
d_recoverHashOK_298 v0
  = case coe v0 of
      C_OpsRecover'46'constructor_4131 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.recoverRefᵇ
d_recoverRef'7495'_300 ::
  T_OpsRecover_294 -> T_RecoverIO'7580'_278 -> Bool
d_recoverRef'7495'_300 v0 v1
  = coe
      d__'38''38'__58
      (coe
         ltInt (coe d_tRecoverR_286 (coe v1))
         (coe d_validityLoR_288 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            d__'61''61''7495'__66 (coe d_depositCountR_290 (coe v1))
            (coe (1 :: Integer)))
         (coe d_recoverHashOK_298 v0 v1))
-- Hydra.Protocol.Reference.MintIOᶜ
d_MintIO'7580'_306 = ()
type T_MintIO'7580'_306 = HsMintIO
pattern C_mkMintIO'7580'_324 a0 a1 a2 a3 = MkMintIO a0 a1 a2 a3
check_mkMintIO'7580'_324 ::
  Integer -> Integer -> Integer -> Integer -> T_MintIO'7580'_306
check_mkMintIO'7580'_324 = MkMintIO
cover_MintIO'7580'_306 :: HsMintIO -> ()
cover_MintIO'7580'_306 x
  = case x of
      MkMintIO _ _ _ _ -> ()
-- Hydra.Protocol.Reference.MintIOᶜ.numPartiesM
d_numPartiesM_316 :: T_MintIO'7580'_306 -> Integer
d_numPartiesM_316 v0
  = case coe v0 of
      C_mkMintIO'7580'_324 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.MintIOᶜ.mintedCountM
d_mintedCountM_318 :: T_MintIO'7580'_306 -> Integer
d_mintedCountM_318 v0
  = case coe v0 of
      C_mkMintIO'7580'_324 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.MintIOᶜ.stQtyM
d_stQtyM_320 :: T_MintIO'7580'_306 -> Integer
d_stQtyM_320 v0
  = case coe v0 of
      C_mkMintIO'7580'_324 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.MintIOᶜ.headTokenCountM
d_headTokenCountM_322 :: T_MintIO'7580'_306 -> Integer
d_headTokenCountM_322 v0
  = case coe v0 of
      C_mkMintIO'7580'_324 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInit
d_OpsInit_326 = ()
newtype T_OpsInit_326
  = C_OpsInit'46'constructor_4269 (T_MintIO'7580'_306 -> Bool)
-- Hydra.Protocol.Reference.OpsInit.initPlacementOK
d_initPlacementOK_330 ::
  T_OpsInit_326 -> T_MintIO'7580'_306 -> Bool
d_initPlacementOK_330 v0
  = case coe v0 of
      C_OpsInit'46'constructor_4269 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.initRefᵇ
d_initRef'7495'_332 :: T_OpsInit_326 -> T_MintIO'7580'_306 -> Bool
d_initRef'7495'_332 v0 v1
  = coe
      d__'38''38'__58
      (coe
         eqInt (coe d_mintedCountM_318 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_numPartiesM_316 (coe v1))))
      (coe
         d__'38''38'__58
         (coe eqInt (coe d_stQtyM_320 (coe v1)) (coe (1 :: Integer)))
         (coe
            d__'38''38'__58
            (coe
               eqInt (coe d_headTokenCountM_322 (coe v1))
               (coe addInt (coe (1 :: Integer)) (coe d_numPartiesM_316 (coe v1))))
            (coe d_initPlacementOK_330 v0 v1)))
-- Hydra.Protocol.Reference.ClaimIOᶜ
d_ClaimIO'7580'_338 = ()
type T_ClaimIO'7580'_338 = HsClaimIO
pattern C_mkClaimIO'7580'_368 a0 a1 a2 a3 a4 a5 a6 = MkClaimIO a0 a1 a2 a3 a4 a5 a6
check_mkClaimIO'7580'_368 ::
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_ClaimIO'7580'_338
check_mkClaimIO'7580'_368 = MkClaimIO
cover_ClaimIO'7580'_338 :: HsClaimIO -> ()
cover_ClaimIO'7580'_338 x
  = case x of
      MkClaimIO _ _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ClaimIOᶜ.tRecoverC
d_tRecoverC_354 :: T_ClaimIO'7580'_338 -> Integer
d_tRecoverC_354 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.validityHiC
d_validityHiC_356 :: T_ClaimIO'7580'_338 -> Integer
d_validityHiC_356 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.depositCidC
d_depositCidC_358 :: T_ClaimIO'7580'_338 -> Integer
d_depositCidC_358 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.headCidC
d_headCidC_360 :: T_ClaimIO'7580'_338 -> Integer
d_headCidC_360 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.headRedeemerIdxC
d_headRedeemerIdxC_362 :: T_ClaimIO'7580'_338 -> Integer
d_headRedeemerIdxC_362 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.claimedRefCodeC
d_claimedRefCodeC_364 :: T_ClaimIO'7580'_338 -> Integer
d_claimedRefCodeC_364 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.ownRefCodeC
d_ownRefCodeC_366 :: T_ClaimIO'7580'_338 -> Integer
d_ownRefCodeC_366 v0
  = case coe v0 of
      C_mkClaimIO'7580'_368 v1 v2 v3 v4 v5 v6 v7 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.claimRefᵇ
d_claimRef'7495'_370 :: T_ClaimIO'7580'_338 -> Bool
d_claimRef'7495'_370 v0
  = coe
      d__'38''38'__58
      (coe
         d__'8804''7470'__84 (coe d_validityHiC_356 (coe v0))
         (coe d_tRecoverC_354 (coe v0)))
      (coe
         d__'38''38'__58
         (coe
            eqInt (coe d_depositCidC_358 (coe v0))
            (coe d_headCidC_360 (coe v0)))
         (coe
            d__'38''38'__58
            (coe
               eqInt (coe d_headRedeemerIdxC_362 (coe v0)) (coe (0 :: Integer)))
            (coe
               eqInt (coe d_claimedRefCodeC_364 (coe v0))
               (coe d_ownRefCodeC_366 (coe v0)))))
-- Hydra.Protocol.Reference.elemᵇ
d_elem'7495'_374 :: Integer -> [Integer] -> Bool
d_elem'7495'_374 v0 v1
  = case coe v1 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__62 (coe eqInt (coe v0) (coe v2))
             (coe d_elem'7495'_374 (coe v0) (coe v3))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.anySharedᵇ
d_anyShared'7495'_382 :: [Integer] -> [Integer] -> Bool
d_anyShared'7495'_382 v0 v1
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      (:) v2 v3
        -> coe
             d__'124''124'__62 (coe d_elem'7495'_374 (coe v2) (coe v1))
             (coe d_anyShared'7495'_382 (coe v3) (coe v1))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.SignerIOᶜ
d_SignerIO'7580'_390 = ()
type T_SignerIO'7580'_390 = HsSignerIO
pattern C_mkSignerIO'7580'_400 a0 a1 = MkSignerIO a0 a1
check_mkSignerIO'7580'_400 ::
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  MAlonzo.Code.Agda.Builtin.List.T_List_10 () Integer ->
  T_SignerIO'7580'_390
check_mkSignerIO'7580'_400 = MkSignerIO
cover_SignerIO'7580'_390 :: HsSignerIO -> ()
cover_SignerIO'7580'_390 x
  = case x of
      MkSignerIO _ _ -> ()
-- Hydra.Protocol.Reference.SignerIOᶜ.signerCodesS
d_signerCodesS_396 :: T_SignerIO'7580'_390 -> [Integer]
d_signerCodesS_396 v0
  = case coe v0 of
      C_mkSignerIO'7580'_400 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.SignerIOᶜ.ptCodesS
d_ptCodesS_398 :: T_SignerIO'7580'_390 -> [Integer]
d_ptCodesS_398 v0
  = case coe v0 of
      C_mkSignerIO'7580'_400 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.participantSignedRefᵇ
d_participantSignedRef'7495'_402 :: T_SignerIO'7580'_390 -> Bool
d_participantSignedRef'7495'_402 v0
  = coe
      d_anyShared'7495'_382 (coe d_signerCodesS_396 (coe v0))
      (coe d_ptCodesS_398 (coe v0))
-- Hydra.Protocol.Reference.AssetIOᶜ
d_AssetIO'7580'_406 = ()
type T_AssetIO'7580'_406 = HsAssetIO
pattern C_mkAssetIO'7580'_420 a0 a1 a2 = MkAssetIO a0 a1 a2
check_mkAssetIO'7580'_420 ::
  Integer -> Integer -> Integer -> T_AssetIO'7580'_406
check_mkAssetIO'7580'_420 = MkAssetIO
cover_AssetIO'7580'_406 :: HsAssetIO -> ()
cover_AssetIO'7580'_406 x
  = case x of
      MkAssetIO _ _ _ -> ()
-- Hydra.Protocol.Reference.AssetIOᶜ.qInA
d_qInA_414 :: T_AssetIO'7580'_406 -> Integer
d_qInA_414 v0
  = case coe v0 of
      C_mkAssetIO'7580'_420 v1 v2 v3 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.AssetIOᶜ.qDeltaA
d_qDeltaA_416 :: T_AssetIO'7580'_406 -> Integer
d_qDeltaA_416 v0
  = case coe v0 of
      C_mkAssetIO'7580'_420 v1 v2 v3 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.AssetIOᶜ.qOutA
d_qOutA_418 :: T_AssetIO'7580'_406 -> Integer
d_qOutA_418 v0
  = case coe v0 of
      C_mkAssetIO'7580'_420 v1 v2 v3 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.perAssetConservedᵇ
d_perAssetConserved'7495'_422 :: [T_AssetIO'7580'_406] -> Bool
d_perAssetConserved'7495'_422 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v1 v2
        -> coe
             d__'38''38'__58
             (coe
                eqInt
                (coe addInt (coe d_qDeltaA_416 (coe v1)) (coe d_qInA_414 (coe v1)))
                (coe d_qOutA_418 (coe v1)))
             (coe d_perAssetConserved'7495'_422 (coe v2))
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.noMintRefᵇ
d_noMintRef'7495'_428 :: Integer -> Bool
d_noMintRef'7495'_428 v0
  = coe d__'61''61''7495'__66 (coe v0) (coe (0 :: Integer))
-- Hydra.Protocol.Reference.refSpentᵇ
d_refSpent'7495'_432 :: Integer -> [Integer] -> Bool
d_refSpent'7495'_432 v0 v1 = coe d_elem'7495'_374 (coe v0) (coe v1)
-- Hydra.Protocol.Reference.partialFanoutRefᵇ
d_partialFanoutRef'7495'_438 ::
  Integer -> Integer -> Integer -> Bool
d_partialFanoutRef'7495'_438 v0 v1 v2
  = coe
      d__'38''38'__58
      (coe d__'60''7495'__78 (coe (0 :: Integer)) (coe v0))
      (coe ltInt (coe v1) (coe v2))
-- Hydra.Protocol.Reference.valuePreservedᵇ
d_valuePreserved'7495'_446 ::
  Integer -> Integer -> Integer -> Integer -> Bool
d_valuePreserved'7495'_446 v0 v1 v2 v3
  = coe
      d__'38''38'__58 (coe eqInt (coe v0) (coe v1))
      (coe eqInt (coe v2) (coe v3))
-- Hydra.Protocol.Reference.contestParamsᵇ
d_contestParams'7495'_456 ::
  Integer -> Integer -> Integer -> Integer -> Bool
d_contestParams'7495'_456 v0 v1 v2 v3
  = coe
      d__'38''38'__58 (coe eqInt (coe v0) (coe v1))
      (coe eqInt (coe v2) (coe v3))
-- Hydra.Protocol.Reference.initHeadIdᵇ
d_initHeadId'7495'_466 :: Integer -> Integer -> Bool
d_initHeadId'7495'_466 v0 v1 = coe eqInt (coe v0) (coe v1)
-- Hydra.Protocol.Reference.BurnIOᶜ
d_BurnIO'7580'_472 = ()
type T_BurnIO'7580'_472 = HsBurnIO
pattern C_mkBurnIO'7580'_482 a0 a1 = MkBurnIO a0 a1
check_mkBurnIO'7580'_482 ::
  Integer -> Integer -> T_BurnIO'7580'_472
check_mkBurnIO'7580'_482 = MkBurnIO
cover_BurnIO'7580'_472 :: HsBurnIO -> ()
cover_BurnIO'7580'_472 x
  = case x of
      MkBurnIO _ _ -> ()
-- Hydra.Protocol.Reference.BurnIOᶜ.mintedCountB
d_mintedCountB_478 :: T_BurnIO'7580'_472 -> Integer
d_mintedCountB_478 v0
  = case coe v0 of
      C_mkBurnIO'7580'_482 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.BurnIOᶜ.burnedCountB
d_burnedCountB_480 :: T_BurnIO'7580'_472 -> Integer
d_burnedCountB_480 v0
  = case coe v0 of
      C_mkBurnIO'7580'_482 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.burnRefᵇ
d_burnRef'7495'_484 :: T_BurnIO'7580'_472 -> Bool
d_burnRef'7495'_484 v0
  = coe
      d__'38''38'__58
      (coe eqInt (coe d_mintedCountB_478 (coe v0)) (coe (0 :: Integer)))
      (coe ltInt (coe (0 :: Integer)) (coe d_burnedCountB_480 (coe v0)))