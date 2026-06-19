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

data HsCloseTag = CloseInitialT | CloseAnyT | CloseUnusedT | CloseUsedT
data HsOpen = MkOpen Integer Integer
data HsClosed = MkClosed Integer Integer Integer Integer
data HsIncIO = MkIncIO Integer Integer Integer Integer Integer
data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer
data HsFanout = MkFanout Integer
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
pattern C_mkClosed'7580'_46 a0 a1 a2 a3 = MkClosed a0 a1 a2 a3
check_mkClosed'7580'_46 ::
  Integer -> Integer -> Integer -> Integer -> T_Closed'7580'_28
check_mkClosed'7580'_46 = MkClosed
cover_Closed'7580'_28 :: HsClosed -> ()
cover_Closed'7580'_28 x
  = case x of
      MkClosed _ _ _ _ -> ()
-- Hydra.Protocol.Reference.Closedᶜ.versionC
d_versionC_38 :: T_Closed'7580'_28 -> Integer
d_versionC_38 v0
  = case coe v0 of
      C_mkClosed'7580'_46 v1 v2 v3 v4 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.cpC
d_cpC_40 :: T_Closed'7580'_28 -> Integer
d_cpC_40 v0
  = case coe v0 of
      C_mkClosed'7580'_46 v1 v2 v3 v4 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.snapshotC
d_snapshotC_42 :: T_Closed'7580'_28 -> Integer
d_snapshotC_42 v0
  = case coe v0 of
      C_mkClosed'7580'_46 v1 v2 v3 v4 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Closedᶜ.contesterLenC
d_contesterLenC_44 :: T_Closed'7580'_28 -> Integer
d_contesterLenC_44 v0
  = case coe v0 of
      C_mkClosed'7580'_46 v1 v2 v3 v4 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Ops
d_Ops_48 = ()
newtype T_Ops_48
  = C_Ops'46'constructor_89 (T_Open'7580'_16 ->
                             T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool)
-- Hydra.Protocol.Reference.Ops.closeCryptoOK
d_closeCryptoOK_52 ::
  T_Ops_48 ->
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
d_closeCryptoOK_52 v0
  = case coe v0 of
      C_Ops'46'constructor_89 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference._&&_
d__'38''38'__54 :: Bool -> Bool -> Bool
d__'38''38'__54 v0 v1 = if coe v0 then coe v1 else coe v0
-- Hydra.Protocol.Reference._==ᵇ_
d__'61''61''7495'__58 :: Integer -> Integer -> Bool
d__'61''61''7495'__58 v0 v1
  = case coe v0 of
      0 -> case coe v1 of
             0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             _ -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                     coe (coe d__'61''61''7495'__58 (coe v2) (coe v3)))
-- Hydra.Protocol.Reference._≤ᵇ_
d__'8804''7495'__64 :: Integer -> Integer -> Bool
d__'8804''7495'__64 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                     coe (coe d__'8804''7495'__64 (coe v2) (coe v3)))
-- Hydra.Protocol.Reference._<ᵇ_
d__'60''7495'__70 :: Integer -> Integer -> Bool
d__'60''7495'__70 v0 v1
  = coe
      d__'8804''7495'__64 (coe addInt (coe (1 :: Integer)) (coe v0))
      (coe v1)
-- Hydra.Protocol.Reference.isNull
d_isNull_78 :: () -> [AgdaAny] -> Bool
d_isNull_78 ~v0 v1 = du_isNull_78 v1
du_isNull_78 :: [AgdaAny] -> Bool
du_isNull_78 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v1 v2 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.closeRefᵇ
d_closeRef'7495'_80 ::
  T_Ops_48 ->
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
d_closeRef'7495'_80 v0 v1 v2 v3
  = coe
      d__'38''38'__54
      (coe
         d__'61''61''7495'__58 (coe d_versionO_22 (coe v1))
         (coe d_versionC_38 (coe v2)))
      (coe
         d__'38''38'__54
         (coe
            d__'61''61''7495'__58 (coe d_cpO_24 (coe v1))
            (coe d_cpC_40 (coe v2)))
         (coe
            d__'38''38'__54
            (coe
               d__'61''61''7495'__58 (coe d_contesterLenC_44 (coe v2))
               (coe (0 :: Integer)))
            (coe
               d__'38''38'__54 (coe du_initialOK_94 (coe v1) (coe v2) (coe v3))
               (coe
                  d__'38''38'__54 (coe du_anyOK_96 (coe v2) (coe v3))
                  (coe d_closeCryptoOK_52 v0 v1 v2 v3)))))
-- Hydra.Protocol.Reference._.initialOK
d_initialOK_94 ::
  T_Ops_48 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> T_CloseTag'7580'_6 -> Bool
d_initialOK_94 ~v0 v1 v2 ~v3 v4 = du_initialOK_94 v1 v2 v4
du_initialOK_94 ::
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_initialOK_94 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v2 of
         C_closeInitial'7580'_8
           -> coe
                d__'38''38'__54
                (coe
                   d__'61''61''7495'__58 (coe d_versionO_22 (coe v0))
                   (coe (0 :: Integer)))
                (coe
                   d__'61''61''7495'__58 (coe d_snapshotC_42 (coe v1))
                   (coe (0 :: Integer)))
         _ -> coe v3)
-- Hydra.Protocol.Reference._.anyOK
d_anyOK_96 ::
  T_Ops_48 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> T_CloseTag'7580'_6 -> Bool
d_anyOK_96 ~v0 ~v1 v2 ~v3 v4 = du_anyOK_96 v2 v4
du_anyOK_96 :: T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_anyOK_96 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v1 of
         C_closeAny'7580'_10
           -> coe
                d__'60''7495'__70 (coe (0 :: Integer))
                (coe d_snapshotC_42 (coe v0))
         _ -> coe v2)
-- Hydra.Protocol.Reference.IncIOᶜ
d_IncIO'7580'_98 = ()
type T_IncIO'7580'_98 = HsIncIO
pattern C_mkIncIO'7580'_120 a0 a1 a2 a3 a4 = MkIncIO a0 a1 a2 a3 a4
check_mkIncIO'7580'_120 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_IncIO'7580'_98
check_mkIncIO'7580'_120 = MkIncIO
cover_IncIO'7580'_98 :: HsIncIO -> ()
cover_IncIO'7580'_98 x
  = case x of
      MkIncIO _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.IncIOᶜ.versionIn
d_versionIn_110 :: T_IncIO'7580'_98 -> Integer
d_versionIn_110 v0
  = case coe v0 of
      C_mkIncIO'7580'_120 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.versionOut
d_versionOut_112 :: T_IncIO'7580'_98 -> Integer
d_versionOut_112 v0
  = case coe v0 of
      C_mkIncIO'7580'_120 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaIn
d_adaIn_114 :: T_IncIO'7580'_98 -> Integer
d_adaIn_114 v0
  = case coe v0 of
      C_mkIncIO'7580'_120 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaDelta
d_adaDelta_116 :: T_IncIO'7580'_98 -> Integer
d_adaDelta_116 v0
  = case coe v0 of
      C_mkIncIO'7580'_120 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaOut
d_adaOut_118 :: T_IncIO'7580'_98 -> Integer
d_adaOut_118 v0
  = case coe v0 of
      C_mkIncIO'7580'_120 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInc
d_OpsInc_122 = ()
newtype T_OpsInc_122
  = C_OpsInc'46'constructor_2081 (T_IncIO'7580'_98 -> Bool)
-- Hydra.Protocol.Reference.OpsInc.incCryptoOK
d_incCryptoOK_126 :: T_OpsInc_122 -> T_IncIO'7580'_98 -> Bool
d_incCryptoOK_126 v0
  = case coe v0 of
      C_OpsInc'46'constructor_2081 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.incRefᵇ
d_incRef'7495'_128 :: T_OpsInc_122 -> T_IncIO'7580'_98 -> Bool
d_incRef'7495'_128 v0 v1
  = coe
      d__'38''38'__54
      (coe
         d__'61''61''7495'__58 (coe d_versionOut_112 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_110 (coe v1))))
      (coe
         d__'38''38'__54
         (coe
            d__'61''61''7495'__58
            (coe
               addInt (coe d_adaDelta_116 (coe v1)) (coe d_adaIn_114 (coe v1)))
            (coe d_adaOut_118 (coe v1)))
         (coe d_incCryptoOK_126 v0 v1))
-- Hydra.Protocol.Reference.decRefᵇ
d_decRef'7495'_134 :: T_OpsInc_122 -> T_IncIO'7580'_98 -> Bool
d_decRef'7495'_134 v0 v1
  = coe
      d__'38''38'__54
      (coe
         d__'61''61''7495'__58 (coe d_versionOut_112 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_110 (coe v1))))
      (coe d_incCryptoOK_126 v0 v1)
-- Hydra.Protocol.Reference.ContestIOᶜ
d_ContestIO'7580'_140 = ()
type T_ContestIO'7580'_140 = HsContestIO
pattern C_mkContestIO'7580'_166 a0 a1 a2 a3 a4 a5 = MkContestIO a0 a1 a2 a3 a4 a5
check_mkContestIO'7580'_166 ::
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_ContestIO'7580'_140
check_mkContestIO'7580'_166 = MkContestIO
cover_ContestIO'7580'_140 :: HsContestIO -> ()
cover_ContestIO'7580'_140 x
  = case x of
      MkContestIO _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ContestIOᶜ.versionInK
d_versionInK_154 :: T_ContestIO'7580'_140 -> Integer
d_versionInK_154 v0
  = case coe v0 of
      C_mkContestIO'7580'_166 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.versionOutK
d_versionOutK_156 :: T_ContestIO'7580'_140 -> Integer
d_versionOutK_156 v0
  = case coe v0 of
      C_mkContestIO'7580'_166 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapIn
d_snapIn_158 :: T_ContestIO'7580'_140 -> Integer
d_snapIn_158 v0
  = case coe v0 of
      C_mkContestIO'7580'_166 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapOut
d_snapOut_160 :: T_ContestIO'7580'_140 -> Integer
d_snapOut_160 v0
  = case coe v0 of
      C_mkContestIO'7580'_166 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenIn
d_contesterLenIn_162 :: T_ContestIO'7580'_140 -> Integer
d_contesterLenIn_162 v0
  = case coe v0 of
      C_mkContestIO'7580'_166 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenOut
d_contesterLenOut_164 :: T_ContestIO'7580'_140 -> Integer
d_contesterLenOut_164 v0
  = case coe v0 of
      C_mkContestIO'7580'_166 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsContest
d_OpsContest_168 = ()
newtype T_OpsContest_168
  = C_OpsContest'46'constructor_2359 (T_ContestIO'7580'_140 -> Bool)
-- Hydra.Protocol.Reference.OpsContest.contestCryptoOK
d_contestCryptoOK_172 ::
  T_OpsContest_168 -> T_ContestIO'7580'_140 -> Bool
d_contestCryptoOK_172 v0
  = case coe v0 of
      C_OpsContest'46'constructor_2359 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.contestRefᵇ
d_contestRef'7495'_174 ::
  T_OpsContest_168 -> T_ContestIO'7580'_140 -> Bool
d_contestRef'7495'_174 v0 v1
  = coe
      d__'38''38'__54
      (coe
         d__'61''61''7495'__58 (coe d_versionInK_154 (coe v1))
         (coe d_versionOutK_156 (coe v1)))
      (coe
         d__'38''38'__54
         (coe
            d__'60''7495'__70 (coe d_snapIn_158 (coe v1))
            (coe d_snapOut_160 (coe v1)))
         (coe
            d__'38''38'__54
            (coe
               d__'61''61''7495'__58 (coe d_contesterLenOut_164 (coe v1))
               (coe
                  addInt (coe (1 :: Integer)) (coe d_contesterLenIn_162 (coe v1))))
            (coe d_contestCryptoOK_172 v0 v1)))
-- Hydra.Protocol.Reference.Fanoutᶜ
d_Fanout'7580'_180 = ()
type T_Fanout'7580'_180 = HsFanout
pattern C_mkFanout'7580'_186 a0 = MkFanout a0
check_mkFanout'7580'_186 :: Integer -> T_Fanout'7580'_180
check_mkFanout'7580'_186 = MkFanout
cover_Fanout'7580'_180 :: HsFanout -> ()
cover_Fanout'7580'_180 x
  = case x of
      MkFanout _ -> ()
-- Hydra.Protocol.Reference.Fanoutᶜ.numOutputsF
d_numOutputsF_184 :: T_Fanout'7580'_180 -> Integer
d_numOutputsF_184 v0
  = case coe v0 of
      C_mkFanout'7580'_186 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsFanout
d_OpsFanout_188 = ()
newtype T_OpsFanout_188
  = C_OpsFanout'46'constructor_2453 (T_Fanout'7580'_180 -> Bool)
-- Hydra.Protocol.Reference.OpsFanout.fanoutCryptoOK
d_fanoutCryptoOK_192 ::
  T_OpsFanout_188 -> T_Fanout'7580'_180 -> Bool
d_fanoutCryptoOK_192 v0
  = case coe v0 of
      C_OpsFanout'46'constructor_2453 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.fanoutRefᵇ
d_fanoutRef'7495'_194 ::
  T_OpsFanout_188 -> T_Fanout'7580'_180 -> Bool
d_fanoutRef'7495'_194 v0 v1
  = coe
      d__'38''38'__54
      (coe
         d__'60''7495'__70 (coe (0 :: Integer))
         (coe d_numOutputsF_184 (coe v1)))
      (coe d_fanoutCryptoOK_192 v0 v1)