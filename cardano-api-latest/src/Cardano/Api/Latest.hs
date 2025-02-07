module Cardano.Api.Latest where

import Cardano.Api qualified

type Era = Cardano.Api.ConwayEra

-- ** AddressInEra

type AddressInEra = Cardano.Api.AddressInEra Era

{-# COMPLETE ByronAddressInEra #-}
pattern ByronAddressInEra :: Cardano.Api.Address Cardano.Api.ByronAddr -> Cardano.Api.AddressInEra
pattern ByronAddressInEra{byronAddress} <-
  Cardano.Api.AddressInEra Cardano.Api.ByronAddressInAnyEra byronAddress
  where
    ByronAddressInEra =
      Cardano.Api.AddressInEra ByronAddressInAnyEra

{-# COMPLETE ShelleyAddressInEra #-}
pattern ShelleyAddressInEra :: Cardano.Api.Address Cardano.Api.ShelleyAddr -> Cardano.Api.AddressInEra
pattern ShelleyAddressInEra{address} <-
  Cardano.Api.AddressInEra Cardano.Api.ShelleyAddressInEra{} address
  where
    ShelleyAddressInEra =
      Cardano.Api.AddressInEra ShelleyAddressInAnyEra

-- ** AddressTypeInEra

type AddressTypeInEra addrType = Cardano.Api.AddressTypeInEra addrType Era

{-# COMPLETE ByronAddressInAnyEra #-}
pattern ByronAddressInAnyEra :: Cardano.Api.AddressTypeInEra Cardano.Api.ByronAddr
pattern ByronAddressInAnyEra <-
  Cardano.Api.ByronAddressInAnyEra
  where
    ByronAddressInAnyEra =
      Cardano.Api.ByronAddressInAnyEra

{-# COMPLETE ShelleyAddressInAnyEra #-}
pattern ShelleyAddressInAnyEra :: Cardano.Api.AddressTypeInEra Cardano.Api.ShelleyAddr
pattern ShelleyAddressInAnyEra <-
  Cardano.Api.ShelleyAddressInEra _
  where
    ShelleyAddressInAnyEra =
      Cardano.Api.ShelleyAddressInEra shelleyBasedEra

-- ** BalancedTxBody

type BalancedTxBody = Cardano.Api.BalancedTxBody Era

{-# COMPLETE BalancedTxBody #-}
pattern BalancedTxBody :: TxBodyContent BuildTx -> TxBody -> TxOut CtxTx -> Coin -> BalancedTxBody
pattern BalancedTxBody{balancedTxBodyContent, balancedTxBody, balancedTxChangeOutput, balancedTxFee} <-
  Cardano.Api.BalancedTxBody balancedTxBodyContent balancedTxBody balancedTxChangeOutput balancedTxFee
  where
    BalancedTxBody =
      Cardano.Api.BalancedTxBody

-- ** KeyWitness

type KeyWitness = Cardano.Api.KeyWitness Era

{-# COMPLETE ShelleyBootstrapWitness #-}
pattern ShelleyBootstrapWitness :: Ledger.BootstrapWitness StandardCrypto -> KeyWitness
pattern ShelleyBootstrapWitness{shelleyBootstrapWitness} <-
  Cardano.Api.Shelley.ShelleyBootstrapWitness _ shelleyBootstrapWitness
  where
    ShelleyBootstrapWitness =
      Cardano.Api.Shelley.ShelleyBootstrapWitness shelleyBasedEra

{-# COMPLETE ShelleyKeyWitness #-}
pattern ShelleyKeyWitness :: Ledger.WitVKey 'Ledger.Witness StandardCrypto -> KeyWitness
pattern ShelleyKeyWitness{shelleyKeyWitness} <-
  Cardano.Api.Shelley.ShelleyKeyWitness _ shelleyKeyWitness
  where
    ShelleyKeyWitness =
      Cardano.Api.Shelley.ShelleyKeyWitness shelleyBasedEra

-- ** PlutusScript

type PlutusScript = Cardano.Api.PlutusScript PlutusScriptV3

{-# COMPLETE PlutusScriptSerialised #-}
pattern PlutusScriptSerialised :: ShortByteString -> PlutusScript
pattern PlutusScriptSerialised{plutusScriptSerialised} <-
  Cardano.Api.Shelley.PlutusScriptSerialised plutusScriptSerialised
  where
    PlutusScriptSerialised =
      Cardano.Api.Shelley.PlutusScriptSerialised

-- ** Script

type Script = Cardano.Api.Script PlutusScriptV3

{-# COMPLETE PlutusScript #-}
pattern PlutusScript :: PlutusScript -> Script
pattern PlutusScript{plutusScript} <-
  Cardano.Api.Shelley.PlutusScript _ plutusScript
  where
    PlutusScript =
      Cardano.Api.Shelley.PlutusScript PlutusScriptV3

-- ** ScriptInEra

type ScriptInEra = Cardano.Api.ScriptInEra Era

-- ** ScriptLanguage

type ScriptLanguage = Cardano.Api.ScriptLanguage PlutusScriptV3

{-# COMPLETE PlutusScriptLanguage #-}
pattern PlutusScriptLanguage :: ScriptLanguage
pattern PlutusScriptLanguage <-
  Cardano.Api.Shelley.PlutusScriptLanguage _
  where
    PlutusScriptLanguage =
      Cardano.Api.Shelley.PlutusScriptLanguage PlutusScriptV3

-- ** ScriptWitness

type ScriptWitness witCtx = Cardano.Api.ScriptWitness witCtx Era

{-# COMPLETE PlutusScriptWitness #-}
pattern PlutusScriptWitness ::
  PlutusScript ->
  ScriptDatum witctx ->
  ScriptRedeemer ->
  ExecutionUnits ->
  ScriptWitness witctx
pattern PlutusScriptWitness
  { plutusScriptWitnessScript
  , plutusScriptWitnessDatum
  , plutusScriptWitnessRedeemer
  , plutusScriptWitnessExecutionUnits
  } <-
  Cardano.Api.PlutusScriptWitness
    _
    PlutusScriptV3
    (PScript plutusScriptWitnessScript)
    plutusScriptWitnessDatum
    plutusScriptWitnessRedeemer
    plutusScriptWitnessExecutionUnits
  where
    PlutusScriptWitness =
      Cardano.Api.PlutusScriptWitness
        scriptLanguageInEra
        PlutusScriptV3
        . PScript

-- ** Tx

type Tx = Cardano.Api.Tx Era

{-# COMPLETE Tx #-}
pattern Tx :: TxBody -> [KeyWitness] -> Tx
pattern Tx{txBody, txKeyWitnesses} <-
  Cardano.Api.Tx txBody txKeyWitnesses
  where
    Tx =
      Cardano.Api.Tx

{-# COMPLETE ShelleyTxBody #-}
pattern ShelleyTxBody ::
  Ledger.TxBody LedgerEra ->
  [Ledger.Script LedgerEra] ->
  TxBodyScriptData ->
  Maybe (Ledger.AlonzoTxAuxData LedgerEra) ->
  TxScriptValidity ->
  TxBody
pattern ShelleyTxBody
  { txBodyLedgerTxBody
  , txBodyScripts
  , txBodyScriptData
  , txBodyAuxiliaryData
  , txBodyScriptValidity
  } <-
  Cardano.Api.Shelley.ShelleyTxBody
    _
    txBodyLedgerTxBody
    txBodyScripts
    txBodyScriptData
    txBodyAuxiliaryData
    txBodyScriptValidity
  where
    ShelleyTxBody =
      Cardano.Api.Shelley.ShelleyTxBody shelleyBasedEra

signShelleyTransaction :: TxBody -> [ShelleyWitnessSigningKey] -> Tx
signShelleyTransaction = Cardano.Api.signShelleyTransaction shelleyBasedEra

-- ** TxAuxScripts

type TxAuxScripts = Cardano.Api.TxAuxScripts Era

{-# COMPLETE TxAuxScriptsNone #-}
pattern TxAuxScriptsNone :: TxAuxScripts
pattern TxAuxScriptsNone <-
  Cardano.Api.TxAuxScriptsNone
  where
    TxAuxScriptsNone =
      Cardano.Api.TxAuxScriptsNone

{-# COMPLETE TxAuxScripts #-}
pattern TxAuxScripts :: [ScriptInEra] -> TxAuxScripts
pattern TxAuxScripts{txAuxScripts'} <-
  Cardano.Api.TxAuxScripts _ txAuxScripts'
  where
    TxAuxScripts =
      Cardano.Api.TxAuxScripts allegraBasedEra

-- ** TxBody

type TxBody = Cardano.Api.TxBody Era

createAndValidateTransactionBody :: TxBodyContent BuildTx -> Either TxBodyError TxBody
createAndValidateTransactionBody = Cardano.Api.createTransactionBody shelleyBasedEra

defaultTxBodyContent :: TxBodyContent BuildTx
defaultTxBodyContent = Cardano.Api.defaultTxBodyContent shelleyBasedEra

-- ** TxBodyContent

type TxBodyContent buidl = Cardano.Api.TxBodyContent buidl Era

{-# COMPLETE TxBodyContent #-}
pattern TxBodyContent ::
  TxIns buidl ->
  TxInsCollateral ->
  TxInsReference ->
  [TxOut CtxTx] ->
  TxTotalCollateral Era ->
  TxReturnCollateral CtxTx Era ->
  TxFee ->
  TxValidityLowerBound ->
  TxValidityUpperBound ->
  TxMetadataInEra ->
  TxAuxScripts ->
  TxExtraKeyWitnesses ->
  BuildTxWith buidl (Maybe (LedgerProtocolParameters Era)) ->
  TxWithdrawals buidl Era ->
  TxCertificates buidl Era ->
  TxUpdateProposal Era ->
  TxMintValue buidl ->
  TxScriptValidity ->
  Maybe (Featured ConwayEraOnwards Era (TxProposalProcedures buidl Era)) ->
  Maybe (Featured ConwayEraOnwards Era (TxVotingProcedures buidl Era)) ->
  Maybe (Featured ConwayEraOnwards Era (Maybe Coin)) ->
  Maybe (Featured ConwayEraOnwards Era Coin) ->
  TxBodyContent buidl
pattern TxBodyContent
  { txIns
  , txInsCollateral
  , txInsReference
  , txOuts
  , txTotalCollateral
  , txReturnCollateral
  , txFee
  , txValidityLowerBound
  , txValidityUpperBound
  , txMetadata
  , txAuxScripts
  , txExtraKeyWits
  , txProtocolParams
  , txWithdrawals
  , txCertificates
  , txUpdateProposal
  , txMintValue
  , txScriptValidity
  , txProposalProcedures
  , txVotingProcedures
  , txCurrentTreasuryValue
  , txTreasuryDonation
  } <-
  Cardano.Api.TxBodyContent
    txIns
    txInsCollateral
    txInsReference
    txOuts
    txTotalCollateral
    txReturnCollateral
    txFee
    txValidityLowerBound
    txValidityUpperBound
    txMetadata
    txAuxScripts
    txExtraKeyWits
    txProtocolParams
    txWithdrawals
    txCertificates
    txUpdateProposal
    txMintValue
    txScriptValidity
    txProposalProcedures
    txVotingProcedures
    txCurrentTreasuryValue
    txTreasuryDonation
  where
    TxBodyContent = Cardano.Api.TxBodyContent

-- ** TxBodyScriptData

type TxBodyScriptData = Cardano.Api.TxBodyScriptData Era

{-# COMPLETE TxBodyNoScriptData #-}
pattern TxBodyNoScriptData :: TxBodyScriptData
pattern TxBodyNoScriptData <-
  Cardano.Api.TxBodyNoScriptData
  where
    TxBodyNoScriptData =
      Cardano.Api.TxBodyNoScriptData

{-# COMPLETE TxBodyScriptData #-}
pattern TxBodyScriptData ::
  Ledger.TxDats (ShelleyLedgerEra Era) ->
  Ledger.Redeemers (ShelleyLedgerEra Era) ->
  TxBodyScriptData
pattern TxBodyScriptData{txBodyScriptDatums, txBodyScriptRedeemers} <-
  Cardano.Api.TxBodyScriptData _ txBodyScriptDatums txBodyScriptRedeemers
  where
    TxBodyScriptData =
      Cardano.Api.TxBodyScriptData alonzoBasedEra

-- ** TxExtraKeyWitnesses

type TxExtraKeyWitnesses = Cardano.Api.TxExtraKeyWitnesses Era

{-# COMPLETE TxExtraKeyWitnessesNone #-}
pattern TxExtraKeyWitnessesNone :: TxExtraKeyWitnesses
pattern TxExtraKeyWitnessesNone <-
  Cardano.Api.TxExtraKeyWitnessesNone
  where
    TxExtraKeyWitnessesNone = Cardano.Api.TxExtraKeyWitnessesNone

{-# COMPLETE TxExtraKeyWitnesses #-}
pattern TxExtraKeyWitnesses :: [Hash PaymentKey] -> TxExtraKeyWitnesses
pattern TxExtraKeyWitnesses{txExtraKeyWitnesses} <-
  Cardano.Api.TxExtraKeyWitnesses _ txExtraKeyWitnesses
  where
    TxExtraKeyWitnesses =
      Cardano.Api.TxExtraKeyWitnesses alonzoBasedEra

-- ** TxFee

type TxFee = Cardano.Api.TxFee Era

{-# COMPLETE TxFeeExplicit #-}
pattern TxFeeExplicit :: Coin -> TxFee
pattern TxFeeExplicit{txFeeExplicit} <-
  Cardano.Api.TxFeeExplicit _ txFeeExplicit
  where
    TxFeeExplicit =
      Cardano.Api.TxFeeExplicit shelleyBasedEra

-- ** TxIns

type TxIns buidl = [(TxIn, BuildTxWith buidl (Cardano.Api.Witness WitCtxTxIn Era))]

-- ** TxInsReference

type TxInsReference = Cardano.Api.TxInsReference Era

{-# COMPLETE TxInsReferenceNone #-}
pattern TxInsReferenceNone :: TxInsReference
pattern TxInsReferenceNone <-
  Cardano.Api.TxInsReferenceNone
  where
    TxInsReferenceNone =
      Cardano.Api.TxInsReferenceNone

{-# COMPLETE TxInsReference #-}
pattern TxInsReference :: [TxIn] -> TxInsReference
pattern TxInsReference{txInsReference'} <-
  Cardano.Api.TxInsReference _ txInsReference'
  where
    TxInsReference =
      Cardano.Api.TxInsReference babbageBasedEra

-- ** TxInsCollateral

type TxInsCollateral = Cardano.Api.TxInsCollateral Era

{-# COMPLETE TxInsCollateralNone #-}
pattern TxInsCollateralNone :: TxInsCollateral
pattern TxInsCollateralNone <-
  Cardano.Api.TxInsCollateralNone
  where
    TxInsCollateralNone =
      Cardano.Api.TxInsCollateralNone

{-# COMPLETE TxInsCollateral #-}
pattern TxInsCollateral :: [TxIn] -> TxInsCollateral
pattern TxInsCollateral{txInsCollateral'} <-
  Cardano.Api.TxInsCollateral _ txInsCollateral'
  where
    TxInsCollateral =
      Cardano.Api.TxInsCollateral alonzoBasedEra

-- ** TxMetadataInEra

type TxMetadataInEra = Cardano.Api.TxMetadataInEra Era

{-# COMPLETE TxMetadataNone #-}
pattern TxMetadataNone :: TxMetadataInEra
pattern TxMetadataNone <-
  Cardano.Api.TxMetadataNone
  where
    TxMetadataNone =
      Cardano.Api.TxMetadataNone

{-# COMPLETE TxMetadataInEra #-}
pattern TxMetadataInEra :: TxMetadata -> TxMetadataInEra
pattern TxMetadataInEra{txMetadataInEra} <-
  Cardano.Api.TxMetadataInEra _ txMetadataInEra
  where
    TxMetadataInEra =
      Cardano.Api.TxMetadataInEra shelleyBasedEra

-- ** TxMintValue

type TxMintValue buidl = Cardano.Api.TxMintValue buidl Era

{-# COMPLETE TxMintValueNone #-}
pattern TxMintValueNone :: TxMintValue buidl
pattern TxMintValueNone <-
  Cardano.Api.TxMintNone
  where
    TxMintValueNone =
      Cardano.Api.TxMintNone

{-# COMPLETE TxMintValue #-}
pattern TxMintValue ::
  Map PolicyId [(AssetName, Quantity, BuildTxWith buidl (ScriptWitness WitCtxMint))] ->
  TxMintValue buidl
pattern TxMintValue{txMintValueInEra} <-
  Cardano.Api.TxMintValue _ txMintValueInEra
  where
    TxMintValue =
      Cardano.Api.TxMintValue maryBasedEra
-- ** TxOut

type TxOut ctx = Cardano.Api.TxOut ctx Era

{-# COMPLETE TxOut #-}
pattern TxOut :: AddressInEra -> Value -> TxOutDatum ctx -> ReferenceScript -> TxOut ctx
pattern TxOut{txOutAddress, txOutValue, txOutDatum, txOutReferenceScript} <-
  Cardano.Api.TxOut
    txOutAddress
    (TxOutValueShelleyBased _ (Extras.fromLedgerValue -> txOutValue))
    txOutDatum
    txOutReferenceScript
  where
    TxOut addr value datum ref =
      Cardano.Api.TxOut
        addr
        (TxOutValueShelleyBased shelleyBasedEra (Extras.toLedgerValue value))
        datum
        ref

-- ** ReferenceScript

type ReferenceScript = Cardano.Api.Shelley.ReferenceScript Era

{-# COMPLETE ReferenceScript #-}
pattern ReferenceScript :: ScriptInAnyLang -> ReferenceScript
pattern ReferenceScript{referenceScript} <-
  Cardano.Api.Shelley.ReferenceScript
    _
    referenceScript
  where
    ReferenceScript =
      Cardano.Api.Shelley.ReferenceScript
        babbageBasedEra

{-# COMPLETE ReferenceScriptNone #-}
pattern ReferenceScriptNone :: Cardano.Api.Shelley.ReferenceScript Era
pattern ReferenceScriptNone <-
  Cardano.Api.Shelley.ReferenceScriptNone
  where
    ReferenceScriptNone =
      Cardano.Api.Shelley.ReferenceScriptNone

-- ** TxOutDatum

type TxOutDatum ctx = Cardano.Api.TxOutDatum ctx Era

{-# COMPLETE TxOutDatumNone #-}
pattern TxOutDatumNone :: TxOutDatum ctx
pattern TxOutDatumNone <-
  Cardano.Api.TxOutDatumNone
  where
    TxOutDatumNone =
      Cardano.Api.TxOutDatumNone

{-# COMPLETE TxOutDatumHash #-}
pattern TxOutDatumHash :: Hash ScriptData -> TxOutDatum ctx
pattern TxOutDatumHash{txOutDatumHash} <-
  Cardano.Api.TxOutDatumHash _ txOutDatumHash
  where
    TxOutDatumHash =
      Cardano.Api.TxOutDatumHash alonzoBasedEra

{-# COMPLETE TxOutSupplementalDatum #-}
pattern TxOutSupplementalDatum :: HashableScriptData -> TxOutDatum CtxTx
pattern TxOutSupplementalDatum{txOutDatumScriptData} <-
  Cardano.Api.TxOutSupplementalDatum _ txOutDatumScriptData
  where
    TxOutSupplementalDatum =
      Cardano.Api.TxOutSupplementalDatum alonzoBasedEra

{-# COMPLETE TxOutDatumInline #-}
pattern TxOutDatumInline :: HashableScriptData -> TxOutDatum ctx
pattern TxOutDatumInline{txOutDatumInlineScriptData} <-
  Cardano.Api.TxOutDatumInline _ txOutDatumInlineScriptData
  where
    TxOutDatumInline =
      Cardano.Api.TxOutDatumInline babbageBasedEra

-- ** TxScriptValidity

type TxScriptValidity = Cardano.Api.TxScriptValidity Era

{-# COMPLETE TxScriptValidityNone #-}
pattern TxScriptValidityNone :: TxScriptValidity
pattern TxScriptValidityNone <-
  Cardano.Api.TxScriptValidityNone
  where
    TxScriptValidityNone =
      Cardano.Api.TxScriptValidityNone

{-# COMPLETE TxScriptValidity #-}
pattern TxScriptValidity :: ScriptValidity -> TxScriptValidity
pattern TxScriptValidity{txScriptValidity'} <-
  Cardano.Api.TxScriptValidity _ txScriptValidity'
  where
    TxScriptValidity =
      Cardano.Api.TxScriptValidity alonzoBasedEra

-- ** TxValidityLowerBound

type TxValidityLowerBound = Cardano.Api.TxValidityLowerBound Era

{-# COMPLETE TxValidityNoLowerBound #-}
pattern TxValidityNoLowerBound :: TxValidityLowerBound
pattern TxValidityNoLowerBound <-
  Cardano.Api.TxValidityNoLowerBound
  where
    TxValidityNoLowerBound =
      Cardano.Api.TxValidityNoLowerBound

{-# COMPLETE TxValidityLowerBound #-}
pattern TxValidityLowerBound :: SlotNo -> TxValidityLowerBound
pattern TxValidityLowerBound{lowerBound} <-
  Cardano.Api.TxValidityLowerBound _ lowerBound
  where
    TxValidityLowerBound =
      Cardano.Api.TxValidityLowerBound allegraBasedEra

-- ** TxValidityUpperBound

type TxValidityUpperBound = Cardano.Api.TxValidityUpperBound Era

{-# COMPLETE TxValidityNoUpperBound #-}
pattern TxValidityNoUpperBound :: TxValidityUpperBound
pattern TxValidityNoUpperBound <-
  Cardano.Api.TxValidityUpperBound _ Nothing
  where
    TxValidityNoUpperBound =
      Cardano.Api.TxValidityUpperBound shelleyBasedEra Nothing

{-# COMPLETE TxValidityUpperBound #-}
pattern TxValidityUpperBound :: SlotNo -> TxValidityUpperBound
pattern TxValidityUpperBound{upperBound} <-
  Cardano.Api.TxValidityUpperBound _ (Just upperBound)
  where
    TxValidityUpperBound =
      Cardano.Api.TxValidityUpperBound shelleyBasedEra . Just

-- ** Witness

type Witness witCtx = Cardano.Api.Witness witCtx Era

{-# COMPLETE KeyWitness #-}
pattern KeyWitness :: KeyWitnessInCtx ctx -> Witness ctx
pattern KeyWitness keyWitnessInCtx <-
  Cardano.Api.KeyWitness keyWitnessInCtx
  where
    KeyWitness = Cardano.Api.KeyWitness

{-# COMPLETE ScriptWitness #-}
pattern ScriptWitness :: ScriptWitnessInCtx ctx -> ScriptWitness ctx -> Witness ctx
pattern ScriptWitness scriptWitnessInCtx scriptWitness <-
  Cardano.Api.ScriptWitness scriptWitnessInCtx scriptWitness
  where
    ScriptWitness = Cardano.Api.ScriptWitness

makeShelleyKeyWitness :: TxBody -> ShelleyWitnessSigningKey -> KeyWitness
makeShelleyKeyWitness = Cardano.Api.makeShelleyKeyWitness shelleyBasedEra
