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
data HsContestIO = MkContestIO Integer Integer Integer Integer Integer Integer
data HsFanout = MkFanout Integer
data HsRecoverIO = MkRecoverIO Integer Integer
data HsMintIO = MkMintIO Integer Integer
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
-- Hydra.Protocol.Reference.isNull
d_isNull_82 :: () -> [AgdaAny] -> Bool
d_isNull_82 ~v0 v1 = du_isNull_82 v1
du_isNull_82 :: [AgdaAny] -> Bool
du_isNull_82 v0
  = case coe v0 of
      [] -> coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10
      (:) v1 v2 -> coe MAlonzo.Code.Agda.Builtin.Bool.C_false_8
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.closeRefᵇ
d_closeRef'7495'_84 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Integer -> Bool
d_closeRef'7495'_84 v0 v1 v2 v3 v4
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
               d__'38''38'__58 (coe du_initialOK_100 (coe v1) (coe v2) (coe v3))
               (coe
                  d__'38''38'__58 (coe du_anyOK_102 (coe v2) (coe v3))
                  (coe
                     d__'38''38'__58 (coe d_closeCryptoOK_56 v0 v1 v2 v3)
                     (coe
                        eqInt (coe d_tfinalC_48 (coe v2))
                        (coe addInt (coe d_cpO_24 (coe v1)) (coe v4))))))))
-- Hydra.Protocol.Reference._.initialOK
d_initialOK_100 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> Integer -> T_CloseTag'7580'_6 -> Bool
d_initialOK_100 ~v0 v1 v2 ~v3 ~v4 v5 = du_initialOK_100 v1 v2 v5
du_initialOK_100 ::
  T_Open'7580'_16 -> T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_initialOK_100 v0 v1 v2
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
d_anyOK_102 ::
  T_Ops_52 ->
  T_Open'7580'_16 ->
  T_Closed'7580'_28 ->
  T_CloseTag'7580'_6 -> Integer -> T_CloseTag'7580'_6 -> Bool
d_anyOK_102 ~v0 ~v1 v2 ~v3 ~v4 v5 = du_anyOK_102 v2 v5
du_anyOK_102 :: T_Closed'7580'_28 -> T_CloseTag'7580'_6 -> Bool
du_anyOK_102 v0 v1
  = let v2 = coe MAlonzo.Code.Agda.Builtin.Bool.C_true_10 in
    coe
      (case coe v1 of
         C_closeAny'7580'_10
           -> coe
                d__'60''7495'__74 (coe (0 :: Integer))
                (coe d_snapshotC_44 (coe v0))
         _ -> coe v2)
-- Hydra.Protocol.Reference.IncIOᶜ
d_IncIO'7580'_104 = ()
type T_IncIO'7580'_104 = HsIncIO
pattern C_mkIncIO'7580'_126 a0 a1 a2 a3 a4 = MkIncIO a0 a1 a2 a3 a4
check_mkIncIO'7580'_126 ::
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_IncIO'7580'_104
check_mkIncIO'7580'_126 = MkIncIO
cover_IncIO'7580'_104 :: HsIncIO -> ()
cover_IncIO'7580'_104 x
  = case x of
      MkIncIO _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.IncIOᶜ.versionIn
d_versionIn_116 :: T_IncIO'7580'_104 -> Integer
d_versionIn_116 v0
  = case coe v0 of
      C_mkIncIO'7580'_126 v1 v2 v3 v4 v5 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.versionOut
d_versionOut_118 :: T_IncIO'7580'_104 -> Integer
d_versionOut_118 v0
  = case coe v0 of
      C_mkIncIO'7580'_126 v1 v2 v3 v4 v5 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaIn
d_adaIn_120 :: T_IncIO'7580'_104 -> Integer
d_adaIn_120 v0
  = case coe v0 of
      C_mkIncIO'7580'_126 v1 v2 v3 v4 v5 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaDelta
d_adaDelta_122 :: T_IncIO'7580'_104 -> Integer
d_adaDelta_122 v0
  = case coe v0 of
      C_mkIncIO'7580'_126 v1 v2 v3 v4 v5 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.IncIOᶜ.adaOut
d_adaOut_124 :: T_IncIO'7580'_104 -> Integer
d_adaOut_124 v0
  = case coe v0 of
      C_mkIncIO'7580'_126 v1 v2 v3 v4 v5 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInc
d_OpsInc_128 = ()
newtype T_OpsInc_128
  = C_OpsInc'46'constructor_2353 (T_IncIO'7580'_104 -> Bool)
-- Hydra.Protocol.Reference.OpsInc.incCryptoOK
d_incCryptoOK_132 :: T_OpsInc_128 -> T_IncIO'7580'_104 -> Bool
d_incCryptoOK_132 v0
  = case coe v0 of
      C_OpsInc'46'constructor_2353 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.incRefᵇ
d_incRef'7495'_134 :: T_OpsInc_128 -> T_IncIO'7580'_104 -> Bool
d_incRef'7495'_134 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionOut_118 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_116 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            eqInt
            (coe
               addInt (coe d_adaDelta_122 (coe v1)) (coe d_adaIn_120 (coe v1)))
            (coe d_adaOut_124 (coe v1)))
         (coe d_incCryptoOK_132 v0 v1))
-- Hydra.Protocol.Reference.decRefᵇ
d_decRef'7495'_140 :: T_OpsInc_128 -> T_IncIO'7580'_104 -> Bool
d_decRef'7495'_140 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionOut_118 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_versionIn_116 (coe v1))))
      (coe
         d__'38''38'__58
         (coe
            eqInt
            (coe
               addInt (coe d_adaOut_124 (coe v1)) (coe d_adaDelta_122 (coe v1)))
            (coe d_adaIn_120 (coe v1)))
         (coe d_incCryptoOK_132 v0 v1))
-- Hydra.Protocol.Reference.ContestIOᶜ
d_ContestIO'7580'_146 = ()
type T_ContestIO'7580'_146 = HsContestIO
pattern C_mkContestIO'7580'_172 a0 a1 a2 a3 a4 a5 = MkContestIO a0 a1 a2 a3 a4 a5
check_mkContestIO'7580'_172 ::
  Integer ->
  Integer ->
  Integer -> Integer -> Integer -> Integer -> T_ContestIO'7580'_146
check_mkContestIO'7580'_172 = MkContestIO
cover_ContestIO'7580'_146 :: HsContestIO -> ()
cover_ContestIO'7580'_146 x
  = case x of
      MkContestIO _ _ _ _ _ _ -> ()
-- Hydra.Protocol.Reference.ContestIOᶜ.versionInK
d_versionInK_160 :: T_ContestIO'7580'_146 -> Integer
d_versionInK_160 v0
  = case coe v0 of
      C_mkContestIO'7580'_172 v1 v2 v3 v4 v5 v6 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.versionOutK
d_versionOutK_162 :: T_ContestIO'7580'_146 -> Integer
d_versionOutK_162 v0
  = case coe v0 of
      C_mkContestIO'7580'_172 v1 v2 v3 v4 v5 v6 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapIn
d_snapIn_164 :: T_ContestIO'7580'_146 -> Integer
d_snapIn_164 v0
  = case coe v0 of
      C_mkContestIO'7580'_172 v1 v2 v3 v4 v5 v6 -> coe v3
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.snapOut
d_snapOut_166 :: T_ContestIO'7580'_146 -> Integer
d_snapOut_166 v0
  = case coe v0 of
      C_mkContestIO'7580'_172 v1 v2 v3 v4 v5 v6 -> coe v4
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenIn
d_contesterLenIn_168 :: T_ContestIO'7580'_146 -> Integer
d_contesterLenIn_168 v0
  = case coe v0 of
      C_mkContestIO'7580'_172 v1 v2 v3 v4 v5 v6 -> coe v5
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.ContestIOᶜ.contesterLenOut
d_contesterLenOut_170 :: T_ContestIO'7580'_146 -> Integer
d_contesterLenOut_170 v0
  = case coe v0 of
      C_mkContestIO'7580'_172 v1 v2 v3 v4 v5 v6 -> coe v6
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsContest
d_OpsContest_174 = ()
newtype T_OpsContest_174
  = C_OpsContest'46'constructor_2649 (T_ContestIO'7580'_146 -> Bool)
-- Hydra.Protocol.Reference.OpsContest.contestCryptoOK
d_contestCryptoOK_178 ::
  T_OpsContest_174 -> T_ContestIO'7580'_146 -> Bool
d_contestCryptoOK_178 v0
  = case coe v0 of
      C_OpsContest'46'constructor_2649 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.contestRefᵇ
d_contestRef'7495'_180 ::
  T_OpsContest_174 -> T_ContestIO'7580'_146 -> Bool
d_contestRef'7495'_180 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'61''61''7495'__62 (coe d_versionInK_160 (coe v1))
         (coe d_versionOutK_162 (coe v1)))
      (coe
         d__'38''38'__58
         (coe
            d__'60''7495'__74 (coe d_snapIn_164 (coe v1))
            (coe d_snapOut_166 (coe v1)))
         (coe
            d__'38''38'__58
            (coe
               d__'61''61''7495'__62 (coe d_contesterLenOut_170 (coe v1))
               (coe
                  addInt (coe (1 :: Integer)) (coe d_contesterLenIn_168 (coe v1))))
            (coe d_contestCryptoOK_178 v0 v1)))
-- Hydra.Protocol.Reference.Fanoutᶜ
d_Fanout'7580'_186 = ()
type T_Fanout'7580'_186 = HsFanout
pattern C_mkFanout'7580'_192 a0 = MkFanout a0
check_mkFanout'7580'_192 :: Integer -> T_Fanout'7580'_186
check_mkFanout'7580'_192 = MkFanout
cover_Fanout'7580'_186 :: HsFanout -> ()
cover_Fanout'7580'_186 x
  = case x of
      MkFanout _ -> ()
-- Hydra.Protocol.Reference.Fanoutᶜ.numOutputsF
d_numOutputsF_190 :: T_Fanout'7580'_186 -> Integer
d_numOutputsF_190 v0
  = case coe v0 of
      C_mkFanout'7580'_192 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsFanout
d_OpsFanout_194 = ()
newtype T_OpsFanout_194
  = C_OpsFanout'46'constructor_2743 (T_Fanout'7580'_186 -> Bool)
-- Hydra.Protocol.Reference.OpsFanout.fanoutCryptoOK
d_fanoutCryptoOK_198 ::
  T_OpsFanout_194 -> T_Fanout'7580'_186 -> Bool
d_fanoutCryptoOK_198 v0
  = case coe v0 of
      C_OpsFanout'46'constructor_2743 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.fanoutRefᵇ
d_fanoutRef'7495'_200 ::
  T_OpsFanout_194 -> T_Fanout'7580'_186 -> Bool
d_fanoutRef'7495'_200 v0 v1
  = coe
      d__'38''38'__58
      (coe
         d__'60''7495'__74 (coe (0 :: Integer))
         (coe d_numOutputsF_190 (coe v1)))
      (coe d_fanoutCryptoOK_198 v0 v1)
-- Hydra.Protocol.Reference.RecoverIOᶜ
d_RecoverIO'7580'_206 = ()
type T_RecoverIO'7580'_206 = HsRecoverIO
pattern C_mkRecoverIO'7580'_216 a0 a1 = MkRecoverIO a0 a1
check_mkRecoverIO'7580'_216 ::
  Integer -> Integer -> T_RecoverIO'7580'_206
check_mkRecoverIO'7580'_216 = MkRecoverIO
cover_RecoverIO'7580'_206 :: HsRecoverIO -> ()
cover_RecoverIO'7580'_206 x
  = case x of
      MkRecoverIO _ _ -> ()
-- Hydra.Protocol.Reference.RecoverIOᶜ.tRecoverR
d_tRecoverR_212 :: T_RecoverIO'7580'_206 -> Integer
d_tRecoverR_212 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_216 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.RecoverIOᶜ.validityLoR
d_validityLoR_214 :: T_RecoverIO'7580'_206 -> Integer
d_validityLoR_214 v0
  = case coe v0 of
      C_mkRecoverIO'7580'_216 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsRecover
d_OpsRecover_218 = ()
newtype T_OpsRecover_218
  = C_OpsRecover'46'constructor_2821 (T_RecoverIO'7580'_206 -> Bool)
-- Hydra.Protocol.Reference.OpsRecover.recoverHashOK
d_recoverHashOK_222 ::
  T_OpsRecover_218 -> T_RecoverIO'7580'_206 -> Bool
d_recoverHashOK_222 v0
  = case coe v0 of
      C_OpsRecover'46'constructor_2821 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.recoverRefᵇ
d_recoverRef'7495'_224 ::
  T_OpsRecover_218 -> T_RecoverIO'7580'_206 -> Bool
d_recoverRef'7495'_224 v0 v1
  = coe
      d__'38''38'__58
      (coe
         ltInt (coe d_tRecoverR_212 (coe v1))
         (coe d_validityLoR_214 (coe v1)))
      (coe d_recoverHashOK_222 v0 v1)
-- Hydra.Protocol.Reference.MintIOᶜ
d_MintIO'7580'_230 = ()
type T_MintIO'7580'_230 = HsMintIO
pattern C_mkMintIO'7580'_240 a0 a1 = MkMintIO a0 a1
check_mkMintIO'7580'_240 ::
  Integer -> Integer -> T_MintIO'7580'_230
check_mkMintIO'7580'_240 = MkMintIO
cover_MintIO'7580'_230 :: HsMintIO -> ()
cover_MintIO'7580'_230 x
  = case x of
      MkMintIO _ _ -> ()
-- Hydra.Protocol.Reference.MintIOᶜ.numPartiesM
d_numPartiesM_236 :: T_MintIO'7580'_230 -> Integer
d_numPartiesM_236 v0
  = case coe v0 of
      C_mkMintIO'7580'_240 v1 v2 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.MintIOᶜ.mintedCountM
d_mintedCountM_238 :: T_MintIO'7580'_230 -> Integer
d_mintedCountM_238 v0
  = case coe v0 of
      C_mkMintIO'7580'_240 v1 v2 -> coe v2
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.OpsInit
d_OpsInit_242 = ()
newtype T_OpsInit_242
  = C_OpsInit'46'constructor_2901 (T_MintIO'7580'_230 -> Bool)
-- Hydra.Protocol.Reference.OpsInit.initPlacementOK
d_initPlacementOK_246 ::
  T_OpsInit_242 -> T_MintIO'7580'_230 -> Bool
d_initPlacementOK_246 v0
  = case coe v0 of
      C_OpsInit'46'constructor_2901 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
-- Hydra.Protocol.Reference.initRefᵇ
d_initRef'7495'_248 :: T_OpsInit_242 -> T_MintIO'7580'_230 -> Bool
d_initRef'7495'_248 v0 v1
  = coe
      d__'38''38'__58
      (coe
         eqInt (coe d_mintedCountM_238 (coe v1))
         (coe addInt (coe (1 :: Integer)) (coe d_numPartiesM_236 (coe v1))))
      (coe d_initPlacementOK_246 v0 v1)