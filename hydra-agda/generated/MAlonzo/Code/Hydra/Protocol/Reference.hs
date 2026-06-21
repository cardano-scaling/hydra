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
data HsClosed = MkClosed Integer Integer Integer Integer Integer
data HsIncIO = MkIncIO Integer Integer Integer Integer Integer
data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer Integer Integer
data HsFanout = MkFanout Integer Integer Integer Integer Integer
data HsRecoverIO = MkRecoverIO Integer Integer
data HsMintIO = MkMintIO Integer Integer
data HsClaimIO = MkClaimIO Integer Integer
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
-- Hydra.Protocol.Reference._==ᵇ_
d__'61''61''7495'__62 :: Integer -> Integer -> Bool
d__'61''61''7495'__62 v0 v1
  = case coe v0 of
      0 -> case coe v1 of
             0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
             _ -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                     coe (coe d__'61''61''7495'__62 (coe v2) (coe v3)))
-- Hydra.Protocol.Reference._≤ᵇ_
d__'8804''7495'__68 :: Integer -> Integer -> Bool
d__'8804''7495'__68 v0 v1
  = case coe v0 of
      0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      _ -> let v2 = subInt (coe v0) (coe (1 :: Integer)) in
           coe
             (case coe v1 of
                0 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
                _ -> let v3 = subInt (coe v1) (coe (1 :: Integer)) in
                     coe (coe d__'8804''7495'__68 (coe v2) (coe v3)))
-- Hydra.Protocol.Reference._<ᵇ_
d__'60''7495'__74 :: Integer -> Integer -> Bool
d__'60''7495'__74 v0 v1
  = coe
      d__'8804''7495'__68 (coe addInt (coe (1 :: Integer)) (coe v0))
      (coe v1)
-- Hydra.Protocol.Reference._≤ᴮ_
d__'8804''7470'__80 :: Integer -> Integer -> Bool
d__'8804''7470'__80 v0 v1
  = coe ltInt (coe v0) (coe addInt (coe (1 :: Integer)) (coe v1))
-- Hydra.Protocol.Reference.closeRefᵇ
d_closeRef'7495'_86 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Integer -> Bool
d_closeRef'7495'_86 v0 v1 v2 v3 v4
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionO_22 (coe v1))
         (coe d_versionC_40 (coe v2)))
      (coe
         d__'38''38'__58
         (coe
            d__'61''61''7495'__62 (coe d_cpO_24 (coe v1))
            (coe d_cpC_42 (coe v2)))
         (coe
            d__'38''38'__58
            (coe
               d__'61''61''7495'__62 (coe d_contesterLenC_46 (coe v2))
               (coe (0 :: Integer)))
            (coe
               d__'38''38'__58 (coe du_initialOK_102 (coe v1) (coe v2) (coe v3))
               (coe
                  d__'38''38'__58 (coe du_anyOK_104 (coe v2) (coe v3))
                  (coe
                     d__'38''38'__58 (coe d_closeCryptoOK_56 v0 v1 v2 v3)
                     (coe
                        eqInt (coe d_tfinalC_48 (coe v2))
                        (coe addInt (coe d_cpO_24 (coe v1)) (coe v4))))))))
-- Hydra.Protocol.Reference._.initialOK
d_initialOK_102 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> Integer -> T_CloseTag'7580'_6 -> Bool
d_initialOK_102 ~v0 v1 v2 ~v3 ~v4 v5 = du_initialOK_102 v1 v2 v5
du_initialOK_102 ::
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_initialOK_102 v0 v1 v2
  = let v3 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v2 of
         C_closeInitial'7580'_8
           -> coe
                d__'38''38'__58
                (coe
                   d__'61''61''7495'__62 (coe d_versionO_22 (coe v0))
                   (coe (0 :: Integer)))
                (coe
                   d__'61''61''7495'__62 (coe d_snapshotC_44 (coe v1))
                   (coe (0 :: Integer)))
         _ -> coe v3)
-- Hydra.Protocol.Reference._.anyOK
d_anyOK_104 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> Integer -> T_CloseTag'7580'_6 -> Bool
d_anyOK_104 ~v0 ~v1 v2 ~v3 ~v4 v5 = du_anyOK_104 v2 v5
du_anyOK_104 :: T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_anyOK_104 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v1 of
         C_closeAny'7580'_10
           -> coe
                d__'60''7495'__74 (coe (0 :: Integer))
                (coe d_snapshotC_44 (coe v0))
         _ -> coe v2)
-- Hydra.Protocol.Reference.IncIOᶜ
d_IncIO'7580'_106 = ()
type T_IncIO'7580'_106 = HsIncIO
pattern C_mkIncIO'7580'_128 a0 a1 a2 a3 a4 = MkIncIO a0 a1 a2 a3 a4
check_mkIncIO'7580'_128 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_IncIO'7580'_106
check_mkIncIO'7580'_128 = MkIncIO
cover_IncIO'7580'_106 :: HsIncIO -> ()
cover_IncIO'7580'_106 x
  = case x of
      MkIncIO _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.IncIOᶜ.versionIn
d_versionIn_118 :: T_IncIO'7580'_106 -> Integer
d_versionIn_118 v0
  = case coe v0 of
      C_mkIncIO'7580'_128 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.versionOut
d_versionOut_120 :: T_IncIO'7580'_106 -> Integer
d_versionOut_120 v0
  = case coe v0 of
      C_mkIncIO'7580'_128 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaIn
d_adaIn_122 :: T_IncIO'7580'_106 -> Integer
d_adaIn_122 v0
  = case coe v0 of
      C_mkIncIO'7580'_128 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaDelta
d_adaDelta_124 :: T_IncIO'7580'_106 -> Integer
d_adaDelta_124 v0
  = case coe v0 of
      C_mkIncIO'7580'_128 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaOut
d_adaOut_126 :: T_IncIO'7580'_106 -> Integer
d_adaOut_126 v0
  = case coe v0 of
      C_mkIncIO'7580'_128 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInc
d_OpsInc_130 = ()
newtype T_OpsInc_130
  = C_OpsInc'46'constructor_2195 (T_IncIO'7580'_106 -> Bool)
-- Hydra.Protocol.Reference.OpsInc.incCryptoOK
d_incCryptoOK_134 :: T_OpsInc_130 -> T_IncIO'7580'_106 -> Bool
d_incCryptoOK_134 v0
  = case coe v0 of
      C_OpsInc'46'constructor_2195 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.incRefᵇ
d_incRef'7495'_136 :: T_OpsInc_130 -> T_IncIO'7580'_106 -> Bool
d_incRef'7495'_136 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionOut_120 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_118 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            eqInt
            (coe
               addInt (coe d_adaDelta_124 (coe v1)) (coe d_adaIn_122 (coe v1)))
            (coe d_adaOut_126 (coe v1)))
         (coe d_incCryptoOK_134 v0 v1))
-- Hydra.Protocol.Reference.decRefᵇ
d_decRef'7495'_142 :: T_OpsInc_130 -> T_IncIO'7580'_106 -> Bool
d_decRef'7495'_142 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionOut_120 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_118 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            eqInt
            (coe
               addInt (coe d_adaOut_126 (coe v1)) (coe d_adaDelta_124 (coe v1)))
            (coe d_adaIn_122 (coe v1)))
         (coe d_incCryptoOK_134 v0 v1))
-- Hydra.Protocol.Reference.ContestIOᶜ
d_ContestIO'7580'_148 = ()
type T_ContestIO'7580'_148 = HsContestIO
pattern C_mkContestIO'7580'_182 a0 a1 a2 a3 a4 a5 a6 a7 = MkContestIO a0 a1 a2 a3 a4 a5 a6 a7
check_mkContestIO'7580'_182 ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_ContestIO'7580'_148
check_mkContestIO'7580'_182 = MkContestIO
cover_ContestIO'7580'_148 :: HsContestIO -> ()
cover_ContestIO'7580'_148 x
  = case x of
      MkContestIO _ _ _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ContestIOᶜ.versionInK
d_versionInK_166 :: T_ContestIO'7580'_148 -> Integer
d_versionInK_166 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.versionOutK
d_versionOutK_168 :: T_ContestIO'7580'_148 -> Integer
d_versionOutK_168 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapIn
d_snapIn_170 :: T_ContestIO'7580'_148 -> Integer
d_snapIn_170 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapOut
d_snapOut_172 :: T_ContestIO'7580'_148 -> Integer
d_snapOut_172 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenIn
d_contesterLenIn_174 :: T_ContestIO'7580'_148 -> Integer
d_contesterLenIn_174 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenOut
d_contesterLenOut_176 :: T_ContestIO'7580'_148 -> Integer
d_contesterLenOut_176 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.tfinalK
d_tfinalK_178 :: T_ContestIO'7580'_148 -> Integer
d_tfinalK_178 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v7
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.validityHiK
d_validityHiK_180 :: T_ContestIO'7580'_148 -> Integer
d_validityHiK_180 v0
  = case coe v0 of
      C_mkContestIO'7580'_182 v1 v2 v3 v4 v5 v6 v7 v8 -> coe v8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsContest
d_OpsContest_184 = ()
newtype T_OpsContest_184
  = C_OpsContest'46'constructor_2603 (T_ContestIO'7580'_148 -> Bool)
-- Hydra.Protocol.Reference.OpsContest.contestCryptoOK
d_contestCryptoOK_188 ::
  T_OpsContest_184 -> T_ContestIO'7580'_148 -> Bool
d_contestCryptoOK_188 v0
  = case coe v0 of
      C_OpsContest'46'constructor_2603 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.contestRefᵇ
d_contestRef'7495'_190 ::
  T_OpsContest_184 -> T_ContestIO'7580'_148 -> Bool
d_contestRef'7495'_190 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionInK_166 (coe v1))
         (coe d_versionOutK_168 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            d__'60''7495'__74 (coe d_snapIn_170 (coe v1))
            (coe d_snapOut_172 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               d__'61''61''7495'__62 (coe d_contesterLenOut_176 (coe v1))
               (coe
                  addInt (coe (1 :: Integer)) (coe d_contesterLenIn_174 (coe v1))))
            (coe
               d__'38''38'__58
               (coe
                  d__'8804''7470'__80 (coe d_validityHiK_180 (coe v1))
                  (coe d_tfinalK_178 (coe v1)))
               (coe d_contestCryptoOK_188 v0 v1))))
-- Hydra.Protocol.Reference.Fanoutᶜ
d_Fanout'7580'_196 = ()
type T_Fanout'7580'_196 = HsFanout
pattern C_mkFanout'7580'_218 a0 a1 a2 a3 a4 = MkFanout a0 a1 a2 a3 a4
check_mkFanout'7580'_218 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_Fanout'7580'_196
check_mkFanout'7580'_218 = MkFanout
cover_Fanout'7580'_196 :: HsFanout -> ()
cover_Fanout'7580'_196 x
  = case x of
      MkFanout _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.Fanoutᶜ.numOutputsF
d_numOutputsF_208 :: T_Fanout'7580'_196 -> Integer
d_numOutputsF_208 v0
  = case coe v0 of
      C_mkFanout'7580'_218 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.burnedCountF
d_burnedCountF_210 :: T_Fanout'7580'_196 -> Integer
d_burnedCountF_210 v0
  = case coe v0 of
      C_mkFanout'7580'_218 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.numPartiesF
d_numPartiesF_212 :: T_Fanout'7580'_196 -> Integer
d_numPartiesF_212 v0
  = case coe v0 of
      C_mkFanout'7580'_218 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.tfinalF
d_tfinalF_214 :: T_Fanout'7580'_196 -> Integer
d_tfinalF_214 v0
  = case coe v0 of
      C_mkFanout'7580'_218 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.Fanoutᶜ.validityLoF
d_validityLoF_216 :: T_Fanout'7580'_196 -> Integer
d_validityLoF_216 v0
  = case coe v0 of
      C_mkFanout'7580'_218 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsFanout
d_OpsFanout_220 = ()
newtype T_OpsFanout_220
  = C_OpsFanout'46'constructor_2805 (T_Fanout'7580'_196 -> Bool)
-- Hydra.Protocol.Reference.OpsFanout.fanoutCryptoOK
d_fanoutCryptoOK_224 ::
  T_OpsFanout_220 -> T_Fanout'7580'_196 -> Bool
d_fanoutCryptoOK_224 v0
  = case coe v0 of
      C_OpsFanout'46'constructor_2805 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.fanoutRefᵇ
d_fanoutRef'7495'_226 ::
  T_OpsFanout_220 -> T_Fanout'7580'_196 -> Bool
d_fanoutRef'7495'_226 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'60''7495'__74 (coe (0 :: Integer))
         (coe d_numOutputsF_208 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            eqInt (coe d_burnedCountF_210 (coe v1))
            (coe addInt (coe (1 :: Integer)) (coe d_numPartiesF_212 (coe v1))))
         (coe
            d__'38''38'__58
            (coe
               ltInt (coe d_tfinalF_214 (coe v1))
               (coe d_validityLoF_216 (coe v1)))
            (coe d_fanoutCryptoOK_224 v0 v1)))
-- Hydra.Protocol.Reference.RecoverIOᶜ
d_RecoverIO'7580'_232 = ()
type T_RecoverIO'7580'_232 = HsRecoverIO
pattern C_mkRecoverIO'7580'_242 a0 a1 = MkRecoverIO a0 a1
check_mkRecoverIO'7580'_242 ::
  Integer -> Integer -> T_RecoverIO'7580'_232
check_mkRecoverIO'7580'_242 = MkRecoverIO
cover_RecoverIO'7580'_232 :: HsRecoverIO -> ()
cover_RecoverIO'7580'_232 x
  = case x of
      MkRecoverIO _ _ -> ()
-- Hydra.Protocol.Reference.RecoverIOᶜ.tRecoverR
d_tRecoverR_238 :: T_RecoverIO'7580'_232 -> Integer
d_tRecoverR_238 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_242 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.RecoverIOᶜ.validityLoR
d_validityLoR_240 :: T_RecoverIO'7580'_232 -> Integer
d_validityLoR_240 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_242 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsRecover
d_OpsRecover_244 = ()
newtype T_OpsRecover_244
  = C_OpsRecover'46'constructor_2909 (T_RecoverIO'7580'_232 -> Bool)
-- Hydra.Protocol.Reference.OpsRecover.recoverHashOK
d_recoverHashOK_248 ::
  T_OpsRecover_244 -> T_RecoverIO'7580'_232 -> Bool
d_recoverHashOK_248 v0
  = case coe v0 of
      C_OpsRecover'46'constructor_2909 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.recoverRefᵇ
d_recoverRef'7495'_250 ::
  T_OpsRecover_244 -> T_RecoverIO'7580'_232 -> Bool
d_recoverRef'7495'_250 v0 v1
  = coe
      d__'38''38'__58
      (coe
         ltInt (coe d_tRecoverR_238 (coe v1))
         (coe d_validityLoR_240 (coe v1)))
      (coe d_recoverHashOK_248 v0 v1)
-- Hydra.Protocol.Reference.MintIOᶜ
d_MintIO'7580'_256 = ()
type T_MintIO'7580'_256 = HsMintIO
pattern C_mkMintIO'7580'_266 a0 a1 = MkMintIO a0 a1
check_mkMintIO'7580'_266 ::
  Integer -> Integer -> T_MintIO'7580'_256
check_mkMintIO'7580'_266 = MkMintIO
cover_MintIO'7580'_256 :: HsMintIO -> ()
cover_MintIO'7580'_256 x
  = case x of
      MkMintIO _ _ -> ()
-- Hydra.Protocol.Reference.MintIOᶜ.numPartiesM
d_numPartiesM_262 :: T_MintIO'7580'_256 -> Integer
d_numPartiesM_262 v0
  = case coe v0 of
      C_mkMintIO'7580'_266 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.MintIOᶜ.mintedCountM
d_mintedCountM_264 :: T_MintIO'7580'_256 -> Integer
d_mintedCountM_264 v0
  = case coe v0 of
      C_mkMintIO'7580'_266 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInit
d_OpsInit_268 = ()
newtype T_OpsInit_268
  = C_OpsInit'46'constructor_2989 (T_MintIO'7580'_256 -> Bool)
-- Hydra.Protocol.Reference.OpsInit.initPlacementOK
d_initPlacementOK_272 ::
  T_OpsInit_268 -> T_MintIO'7580'_256 -> Bool
d_initPlacementOK_272 v0
  = case coe v0 of
      C_OpsInit'46'constructor_2989 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.initRefᵇ
d_initRef'7495'_274 :: T_OpsInit_268 -> T_MintIO'7580'_256 -> Bool
d_initRef'7495'_274 v0 v1
  = coe
      d__'38''38'__58
      (coe
         eqInt (coe d_mintedCountM_264 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_numPartiesM_262 (coe v1))))
      (coe d_initPlacementOK_272 v0 v1)
-- Hydra.Protocol.Reference.ClaimIOᶜ
d_ClaimIO'7580'_280 = ()
type T_ClaimIO'7580'_280 = HsClaimIO
pattern C_mkClaimIO'7580'_290 a0 a1 = MkClaimIO a0 a1
check_mkClaimIO'7580'_290 ::
  Integer -> Integer -> T_ClaimIO'7580'_280
check_mkClaimIO'7580'_290 = MkClaimIO
cover_ClaimIO'7580'_280 :: HsClaimIO -> ()
cover_ClaimIO'7580'_280 x
  = case x of
      MkClaimIO _ _ -> ()
-- Hydra.Protocol.Reference.ClaimIOᶜ.tRecoverC
d_tRecoverC_286 :: T_ClaimIO'7580'_280 -> Integer
d_tRecoverC_286 v0
  = case coe v0 of
      C_mkClaimIO'7580'_290 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ClaimIOᶜ.validityHiC
d_validityHiC_288 :: T_ClaimIO'7580'_280 -> Integer
d_validityHiC_288 v0
  = case coe v0 of
      C_mkClaimIO'7580'_290 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsClaim
d_OpsClaim_292 = ()
newtype T_OpsClaim_292
  = C_OpsClaim'46'constructor_3071 (T_ClaimIO'7580'_280 -> Bool)
-- Hydra.Protocol.Reference.OpsClaim.claimIncrementOK
d_claimIncrementOK_296 ::
  T_OpsClaim_292 -> T_ClaimIO'7580'_280 -> Bool
d_claimIncrementOK_296 v0
  = case coe v0 of
      C_OpsClaim'46'constructor_3071 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.claimRefᵇ
d_claimRef'7495'_298 ::
  T_OpsClaim_292 -> T_ClaimIO'7580'_280 -> Bool
d_claimRef'7495'_298 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'8804''7470'__80 (coe d_validityHiC_288 (coe v1))
         (coe d_tRecoverC_286 (coe v1)))
      (coe d_claimIncrementOK_296 v0 v1)