-- | A Haskell API for Cardano, tailored to the Hydra project.
--
-- This package provides a wrapper around the @cardano-ledger@, @cardano-api@ and
-- @plutus@ libraries with extra utilities and function commonly used across the
-- Hydra project.
--
-- NOTE: We always use the **latest era** available in our codebase, so to ease
-- type signatures and notations, we specialize any type of the @cardano-api@
-- normally parameterized by an era to the latest era 'Era'. As a consequence,
-- we've defined pattern synonyms for most constructors in the @cardano-api@ to
-- also get rid of era witnesses.
--
-- NOTE: This module also uses the **latest plutus version** available
-- (currently 'PlutusScriptV3'). So make sure that you give it a plutus script
-- of the right version (e.g. when compiling and serializing plutus-tx).
module Hydra.Cardano.Api (
  -- * Common type-alias
  Era,
  LedgerEra,
  ledgerEraVersion,
  LedgerProtocolParameters (..),

  -- * Wrapped Types
  module Hydra.Cardano.Api,

  -- ** UTxO
  UTxO,
  UTxO' (UTxO),

  -- * Extras
  module Extras,

  -- * Re-exports from @cardano-api@
  module X,
) where

import Cardano.Api as X
    ( NetworkMagic(..),
      FromJSON,
      ToJSON,
      ConwayEra,
      CtxUTxO,
      TxIn(..),
      Value,
      anyAddressInEra,
      anyAddressInShelleyBasedEra,
      byronAddressInEra,
      isKeyAddress,
      lexPlausibleAddressString,
      makeByronAddress,
      makeByronAddressInEra,
      makeShelleyAddress,
      makeShelleyAddressInEra,
      makeStakeAddress,
      parseAddressAny,
      shelleyAddressInEra,
      stakeAddressCredential,
      toAddressAny,
      Block,
      chainPointToHeaderHash,
      chainPointToSlotNo,
      chainTipToChainPoint,
      getBlockHeader,
      getBlockTxs,
      makeChainTip,
      deserialiseFromBech32Cip129,
      deserialiseGovActionIdFromBech32Cip129,
      serialiseGovActionIdToBech32Cip129,
      serialiseToBech32Cip129,
      getAnchorDataFromCertificate,
      isDRepRegOrUpdateCert,
      makeCommitteeColdkeyResignationCertificate,
      makeCommitteeHotKeyAuthorizationCertificate,
      makeDrepRegistrationCertificate,
      makeDrepUnregistrationCertificate,
      makeDrepUpdateCertificate,
      makeGenesisKeyDelegationCertificate,
      makeMIRCertificate,
      makeStakeAddressAndDRepDelegationCertificate,
      makeStakeAddressDelegationCertificate,
      makeStakeAddressRegistrationCertificate,
      makeStakeAddressUnregistrationCertificate,
      makeStakePoolRegistrationCertificate,
      makeStakePoolRetirementCertificate,
      selectStakeCredentialWitness,
      constructBalancedTx,
      notScriptLockedTxIns,
      renderNotScriptLockedTxInsError,
      renderTxInsExistError,
      txInsExistInUTxO,
      determineEra,
      executeQueryAnyMode,
      executeQueryCardanoMode,
      queryStateForBalancedTx,
      renderQueryConvenienceError,
      hashDRepMetadata,
      deserialiseAnyVerificationKey,
      deserialiseAnyVerificationKeyBech32,
      deserialiseAnyVerificationKeyTextEnvelope,
      deserialiseInput,
      deserialiseInputAnyOf,
      mapSomeAddressVerificationKey,
      renderInputDecodeError,
      renderSomeAddressVerificationKey,
      alonzoEraOnwardsConstraints,
      alonzoEraOnwardsToShelleyBasedEra,
      babbageEraOnwardsConstraints,
      babbageEraOnwardsToShelleyBasedEra,
      byronToAlonzoEraConstraints,
      conwayEraOnwardsConstraints,
      conwayEraOnwardsToBabbageEraOnwards,
      conwayEraOnwardsToShelleyBasedEra,
      maryEraOnwardsConstraints,
      maryEraOnwardsToShelleyBasedEra,
      forShelleyBasedEraInEon,
      forShelleyBasedEraInEonMaybe,
      forShelleyBasedEraMaybeEon,
      inAnyShelleyBasedEra,
      inEonForShelleyBasedEra,
      inEonForShelleyBasedEraMaybe,
      requireShelleyBasedEra,
      shelleyBasedEraConstraints,
      shelleyEraOnlyConstraints,
      shelleyEraOnlyToShelleyBasedEra,
      shelleyToAllegraEraConstraints,
      shelleyToAllegraEraToShelleyBasedEra,
      shelleyToAlonzoEraConstraints,
      shelleyToAlonzoEraToShelleyBasedEra,
      shelleyToBabbageEraConstraints,
      shelleyToBabbageEraToShelleyBasedEra,
      shelleyToMaryEraConstraints,
      shelleyToMaryEraToShelleyBasedEra,
      alonzoEraOnwardsToMaryEraOnwards,
      babbageEraOnwardsToAlonzoEraOnwards,
      babbageEraOnwardsToMaryEraOnwards,
      caseByronOrShelleyBasedEra,
      caseByronToAlonzoOrBabbageEraOnwards,
      caseShelleyEraOnlyOrAllegraEraOnwards,
      caseShelleyToAllegraOrMaryEraOnwards,
      caseShelleyToAlonzoOrBabbageEraOnwards,
      caseShelleyToBabbageOrConwayEraOnwards,
      caseShelleyToMaryOrAlonzoEraOnwards,
      shelleyToAlonzoEraToShelleyToBabbageEra,
      anyCardanoEra,
      cardanoEraConstraints,
      forEraInEon,
      forEraInEonMaybe,
      forEraMaybeEon,
      inAnyCardanoEra,
      inEonForEraMaybe,
      maybeEon,
      monoidForEraInEon,
      monoidForEraInEonA,
      throwErrorAsException,
      asFeaturedInEra,
      asFeaturedInShelleyBasedEra,
      mkFeatured,
      unFeatured,
      calculateMinTxFee,
      calculateMinimumUTxO,
      estimateBalancedTxBody,
      estimateOrCalculateBalancedTxBody,
      estimateTransactionKeyWitnessCount,
      evaluateTransactionBalance,
      evaluateTransactionExecutionUnits,
      evaluateTransactionFee,
      makeTransactionBodyAutoBalance,
      getAnchorDataFromGovernanceAction,
      validateGovActionAnchorData,
      asType,
      renderSafeHashAsHex,
      intoFile,
      mapFile,
      onlyIn,
      onlyOut,
      readByteStringFile,
      readLazyByteStringFile,
      readTextFile,
      writeByteStringFile,
      writeByteStringFileWithOwnerPermissions,
      writeByteStringOutput,
      writeLazyByteStringFile,
      writeLazyByteStringFileWithOwnerPermissions,
      writeLazyByteStringOutput,
      writeTextFile,
      writeTextFileWithOwnerPermissions,
      writeTextOutput,
      writeSecrets,
      connectToLocalNode,
      connectToLocalNodeWithVersion,
      getLocalChainTip,
      mkLocalNodeClientParams,
      queryNodeLocalState,
      queryTxMonitoringLocal,
      submitTxToNodeLocal,
      executeLocalStateQueryExpr,
      queryExpr,
      generateInsecureSigningKey,
      generateSigningKey,
      autocompleteMnemonicPrefix,
      findMnemonicWordsWithPrefix,
      generateMnemonic,
      signingKeyFromMnemonic,
      signingKeyFromMnemonicWithPaymentKeyIndex,
      readKeyFile,
      readKeyFileAnyOf,
      readKeyFileTextEnvelope,
      applyBlock,
      chainSyncClientPipelinedWithLedgerState,
      chainSyncClientWithLedgerState,
      decodeLedgerState,
      encodeLedgerState,
      envSecurityParam,
      foldBlocks,
      foldEpochState,
      fromConditionResult,
      genesisConfigToEnv,
      getAnyNewEpochState,
      getLedgerTablesUTxOValues,
      initialLedgerState,
      mkProtocolInfoCardano,
      readAlonzoGenesisConfig,
      readByronGenesisConfig,
      readCardanoGenesisConfig,
      readConwayGenesisConfig,
      readNodeConfig,
      readShelleyGenesisConfig,
      shelleyPraosNonce,
      toConditionResult,
      handleIOExceptionsLiftWith,
      handleIOExceptionsWith,
      hoistIOEither,
      liftExceptT,
      liftMaybe,
      modifyError,
      fromNetworkMagic,
      toNetworkMagic,
      getHotKey,
      getKesPeriod,
      getOpCertCount,
      issueOperationalCertificate,
      collectPlutusScriptHashes,
      renderDebugPlutusFailure,
      black,
      cyan,
      docToLazyText,
      docToString,
      docToText,
      magenta,
      prettyException,
      pshow,
      white,
      yellow,
      reflBlockType,
      fromAlonzoCostModel,
      makePraosNonce,
      makeShelleyUpdateProposal,
      toAlonzoCostModel,
      toAlonzoCostModels,
      getProgress,
      getSlotForRelativeTime,
      slotToEpoch,
      toLedgerEpochInfo,
      queryAccountState,
      queryChainBlockNo,
      queryChainPoint,
      queryCommitteeMembersState,
      queryConstitution,
      queryConstitutionHash,
      queryCurrentEpochState,
      queryCurrentEra,
      queryDRepStakeDistribution,
      queryDRepState,
      queryDebugLedgerState,
      queryEpoch,
      queryFuturePParams,
      queryGenesisParameters,
      queryGovState,
      queryLedgerConfig,
      queryLedgerPeerSnapshot,
      queryPoolDistribution,
      queryPoolState,
      queryProposals,
      queryProtocolState,
      queryRatifyState,
      querySPOStakeDistribution,
      queryStakeAddresses,
      queryStakeDelegDeposits,
      queryStakeDistribution,
      queryStakePoolDefaultVote,
      queryStakePoolParameters,
      queryStakeSnapshot,
      queryStakeVoteDelegatees,
      queryUtxo,
      mergeDelegsAndRewards,
      eraOfScriptInEra,
      eraOfScriptLanguageInEra,
      examplePlutusScriptAlwaysFails,
      examplePlutusScriptAlwaysSucceeds,
      getScriptWitnessReferenceInput,
      getScriptWitnessReferenceInputOrScript,
      getScriptWitnessScript,
      hashScript,
      languageOfScriptLanguageInEra,
      sbeToSimpleScriptLanguageInEra,
      toScriptInAnyLang,
      toScriptInEra,
      getOriginalScriptDataBytes,
      getScriptData,
      hashScriptDataBytes,
      scriptDataFromJson,
      scriptDataJsonToHashable,
      scriptDataToJson,
      unsafeHashableScriptData,
      validateScriptData,
      deserialiseAnyOfFromBech32,
      deserialiseFromBech32,
      serialiseToBech32,
      deserialiseFromJSON,
      prettyPrintJSON,
      readFileJSON,
      serialiseToJSON,
      writeFileJSON,
      deserialiseByronTxCddl,
      deserialiseFromTextEnvelopeCddlAnyOf,
      deserialiseWitnessLedgerCddl,
      readFileTextEnvelopeCddlAnyOf,
      serialiseWitnessLedgerCddl,
      writeTxFileTextEnvelopeCanonicalCddl,
      writeTxFileTextEnvelopeCddl,
      writeTxWitnessFileTextEnvelopeCddl,
      deserialiseFromRawBytesHex,
      serialiseToRawBytesHex,
      serialiseToRawBytesHexText,
      deserialiseFromTextEnvelope,
      deserialiseFromTextEnvelopeAnyOf,
      readFileTextEnvelope,
      readFileTextEnvelopeAnyOf,
      readTextEnvelopeFromFile,
      readTextEnvelopeOfTypeFromFile,
      serialiseToTextEnvelope,
      textEnvelopeRawCBOR,
      textEnvelopeToJSON,
      textEnvelopeTypeInEra,
      writeFileTextEnvelope,
      validateAndHashStakePoolMetadata,
      addTxExtraKeyWits,
      addTxIn,
      addTxInCollateral,
      addTxInReference,
      addTxIns,
      addTxInsCollateral,
      addTxInsReference,
      addTxMintValue,
      addTxOut,
      addTxOuts,
      collectTxBodyScriptWitnesses,
      convProposalProcedures,
      createTransactionBody,
      defaultTxFee,
      defaultTxValidityUpperBound,
      genesisUTxOPseudoTxIn,
      getReferenceInputsSizeForTxIds,
      getTxBodyContent,
      getTxId,
      getTxIdByron,
      indexTxMintValue,
      makeByronTransactionBody,
      mkTxCertificates,
      mkTxMintValue,
      mkTxProposalProcedures,
      mkTxVotingProcedures,
      modTxAuxScripts,
      modTxCertificates,
      modTxExtraKeyWits,
      modTxFee,
      modTxIns,
      modTxInsCollateral,
      modTxInsReference,
      modTxMetadata,
      modTxMintValue,
      modTxOuts,
      modTxReturnCollateral,
      modTxScriptValidity,
      modTxTotalCollateral,
      modTxUpdateProposal,
      modTxValidityLowerBound,
      modTxValidityUpperBound,
      modTxWithdrawals,
      renderScriptWitnessIndex,
      setTxAuxScripts,
      setTxCertificates,
      setTxCurrentTreasuryValue,
      setTxExtraKeyWits,
      setTxFee,
      setTxIns,
      setTxInsCollateral,
      setTxInsReference,
      setTxMetadata,
      setTxMintValue,
      setTxOuts,
      setTxProposalProcedures,
      setTxProtocolParams,
      setTxReturnCollateral,
      setTxScriptValidity,
      setTxTotalCollateral,
      setTxTreasuryDonation,
      setTxUpdateProposal,
      setTxValidityLowerBound,
      setTxValidityUpperBound,
      setTxVotingProcedures,
      setTxWithdrawals,
      subtractTxMintValue,
      txMintValueToValue,
      buildTxWithToMaybe,
      fromCtxUTxOTxOut,
      fromLedgerTxOuts,
      lovelaceToTxOutValue,
      parseHash,
      toCtxUTxOTxOut,
      txOutInAnyEra,
      txOutValueToLovelace,
      txOutValueToValue,
      getTxBody,
      getTxWitnesses,
      makeByronKeyWitness,
      makeShelleyBasedBootstrapWitness,
      makeShelleyBootstrapWitness,
      makeShelleyKeyWitness',
      makeSignedByronTransaction,
      makeSignedTransaction,
      signByronTransaction,
      txScriptValidityToScriptValidity,
      renderTxIn,
      makeTransactionMetadata,
      mergeTransactionMetadata,
      metaBytesChunks,
      metaTextChunks,
      metadataFromJson,
      metadataToJson,
      metadataValueFromJsonNoSchema,
      metadataValueToJsonNoSchema,
      validateTxMetadata,
      runParsecParser,
      textShow,
      unsafeBoundedRational,
      filterValue,
      lovelaceToQuantity,
      lovelaceToValue,
      multiAssetToPolicyAssets,
      negateValue,
      policyAssetsToValue,
      quantityToLovelace,
      renderMultiAsset,
      renderMultiAssetPretty,
      renderValue,
      renderValuePretty,
      scriptPolicyId,
      selectAsset,
      selectLovelace,
      valueFromList,
      valueFromNestedRep,
      valueToList,
      valueToLovelace,
      valueToNestedRep,
      valueToPolicyAssets,
      parseAssetName,
      parseMintingMultiAssetValue,
      parsePolicyId,
      parseTxOutMultiAssetValue,
      parseUTxOValue,
      liftEither,
      hsep,
      vsep,
      catchE,
      except,
      finallyE,
      handleE,
      liftCallCC,
      liftListen,
      liftPass,
      mapExcept,
      mapExceptT,
      runExcept,
      runExceptT,
      throwE,
      tryE,
      withExcept,
      withExceptT,
      bimapExceptT,
      bracketExceptT,
      bracketExceptionT,
      catchExceptT,
      catchIOExceptT,
      catchLeftT,
      catchesExceptT,
      exceptT,
      firstExceptT,
      handleExceptT,
      handleIOExceptT,
      handleLeftT,
      handlesExceptT,
      hoistEither,
      hoistExceptT,
      hoistMaybe,
      hushM,
      left,
      newExceptT,
      onLeft,
      onNothing,
      right,
      secondExceptT,
      MonadIO(..),
      Address(..),
      AddressAny(..),
      ByronAddr,
      PaymentCredential(..),
      SerialiseAddress(..),
      ShelleyAddr,
      StakeAddress,
      StakeAddressPointer(..),
      StakeAddressReference(..),
      StakeCredential,
      AnchorDataHash(..),
      AnchorUrl(..),
      Block(..),
      BlockHeader(..),
      BlockInMode(..),
      ChainPoint(..),
      ChainTip(..),
      Cip129(..),
      AnchorDataFromCertificateError(..),
      Certificate(..),
      CommitteeColdkeyResignationRequirements(..),
      CommitteeHotKeyAuthorizationRequirements(..),
      DRepMetadataReference,
      DRepRegistrationRequirements(..),
      DRepUnregistrationRequirements(..),
      DRepUpdateRequirements(..),
      GenesisKeyDelegationRequirements(..),
      MirCertificateRequirements(..),
      StakeAddressRequirements(..),
      StakeDelegationRequirements(..),
      StakePoolMetadataReference,
      StakePoolParameters,
      StakePoolRegistrationRequirements(..),
      StakePoolRelay,
      StakePoolRetirementRequirements(..),
      ScriptLockedTxInsError(..),
      TxInsExistError(..),
      QueryConvenienceError(..),
      TxCurrentTreasuryValue(..),
      DRepMetadata,
      InputDecodeError(..),
      InputFormat(..),
      SomeAddressVerificationKey(..),
      AllegraEraOnwards(..),
      IsAllegraBasedEra(..),
      AlonzoEraOnwards(..),
      AlonzoEraOnwardsConstraints,
      IsAlonzoBasedEra(..),
      BabbageEraOnwards(..),
      IsBabbageBasedEra(..),
      ByronToAlonzoEra(..),
      Convert(..),
      ConwayEraOnwards(..),
      IsConwayBasedEra(..),
      IsMaryBasedEra(..),
      MaryEraOnwards(..),
      AnyShelleyBasedEra(..),
      InAnyShelleyBasedEra(..),
      IsShelleyBasedEra(..),
      ShelleyBasedEra(..),
      ShelleyEraOnly(..),
      ShelleyToAllegraEra(..),
      ShelleyToAlonzoEra(..),
      ShelleyToBabbageEra(..),
      ShelleyToMaryEra(..),
      AllegraEra,
      AlonzoEra,
      AnyCardanoEra(..),
      BabbageEra,
      ByronEra,
      CardanoEra(..),
      Eon(..),
      EraInEon(..),
      InAnyCardanoEra(..),
      IsCardanoEra(..),
      MaryEra,
      ShelleyEra,
      ToCardanoEra(..),
      Error(..),
      FileError(..),
      Featured(..),
      AutoBalanceError(..),
      FeeEstimationMode(..),
      RequiredByronKeyWitnesses(..),
      RequiredShelleyKeyWitnesses(..),
      ResolvablePointers(..),
      ScriptExecutionError(..),
      TotalReferenceScriptsSize(..),
      TransactionValidityError(..),
      TxBodyErrorAutoBalance(..),
      TxFeeEstimationError(..),
      AlonzoGenesisFile,
      ByronGenesisFile,
      ConwayGenesisFile,
      GenesisHashAlonzo(..),
      GenesisHashByron(..),
      GenesisHashConway(..),
      GenesisHashShelley(..),
      ShelleyConfig(..),
      ShelleyGenesisFile,
      GenesisParameters(..),
      CIP119(..),
      CIP108(..),
      AsType(AsAddress, AsPolicyId, AsAssetName, AsTxMetadata, AsTxId,
             AsTxBody, AsTx, AsShelleyWitness, AsShelleyTxBody, AsMaryTxBody,
             AsMaryTx, AsKeyWitness, AsByronWitness, AsByronTxBody, AsAlonzoTx,
             AsAllegraTx, AsStakePoolMetadata, AsTextEnvelope, AsScriptData,
             AsHashableScriptData, AsSimpleScript, AsScriptInEra,
             AsScriptInAnyLang, AsScriptHash, AsScript, AsPlutusScriptV3,
             AsPlutusScriptV2, AsPlutusScriptV1, AsPlutusScriptInEra,
             AsPlutusScript, AsUpdateProposal, AsPraosNonce,
             AsHotCommitteeCredential, AsDrepCredential,
             AsColdCommitteeCredential, AsOperationalCertificateIssueCounter,
             AsOperationalCertificate, AsStakePoolKey, AsStakePoolExtendedKey,
             AsStakeKey, AsStakeExtendedKey, AsPaymentKey, AsPaymentExtendedKey,
             AsGenesisUTxOKey, AsGenesisKey, AsGenesisExtendedKey,
             AsGenesisDelegateKey, AsGenesisDelegateExtendedKey, AsDRepKey,
             AsDRepExtendedKey, AsCommitteeHotKey, AsCommitteeHotExtendedKey,
             AsCommitteeColdKey, AsCommitteeColdExtendedKey, AsVrfKey, AsKesKey,
             AsVerificationKey, AsSigningKey, AsByronKeyLegacy, AsByronKey,
             AsHash, AsVotingProcedures, AsVote, AsProposal, AsShelleyEra,
             AsMaryEra, AsConwayEra, AsByronEra, AsBabbageEra, AsAlonzoEra,
             AsAllegraEra, AsDRepMetadata, AsCertificate, AsStakeAddress,
             AsShelleyAddress, AsShelleyAddr, AsByronAddress, AsByronAddr,
             AsAddressInEra, AsAddressAny),
      FromSomeType(..),
      HasTypeProxy(..),
      castHash,
      Hash,
      File(..),
      FileDirection(..),
      SocketPath,
      LocalChainSyncClient(..),
      LocalNodeClientParams(..),
      LocalNodeClientProtocols(..),
      LocalNodeClientProtocolsInMode,
      LocalNodeConnectInfo(..),
      LocalTxMonitoringQuery(..),
      LocalTxMonitoringResult(..),
      LocalStateQueryExpr,
      UnsupportedNtcVersionError(..),
      TxIdInMode(..),
      TxInMode(..),
      TxValidationErrorInCardanoMode(..),
      ByronKey,
      ByronKeyLegacy,
      castSigningKey,
      castVerificationKey,
      SigningKey(ByronSigningKey, StakeSigningKey, StakePoolSigningKey,
                 StakePoolExtendedSigningKey, StakeExtendedSigningKey,
                 PaymentSigningKey, PaymentExtendedSigningKey,
                 GenesisUTxOSigningKey, GenesisSigningKey,
                 GenesisExtendedSigningKey, GenesisDelegateSigningKey,
                 GenesisDelegateExtendedSigningKey, DRepSigningKey,
                 DRepExtendedSigningKey, CommitteeHotSigningKey,
                 CommitteeHotExtendedSigningKey, CommitteeColdSigningKey,
                 CommitteeColdExtendedSigningKey, ByronSigningKeyLegacy),
      VerificationKey(ByronVerificationKey, StakeVerificationKey,
                      StakePoolVerificationKey, StakePoolExtendedVerificationKey,
                      StakeExtendedVerificationKey, PaymentVerificationKey,
                      PaymentExtendedVerificationKey, GenesisVerificationKey,
                      GenesisUTxOVerificationKey, GenesisExtendedVerificationKey,
                      GenesisDelegateVerificationKey,
                      GenesisDelegateExtendedVerificationKey, DRepVerificationKey,
                      DRepExtendedVerificationKey, CommitteeHotVerificationKey,
                      CommitteeHotExtendedVerificationKey, CommitteeColdVerificationKey,
                      CommitteeColdExtendedVerificationKey, ByronVerificationKeyLegacy,
                      unStakeVerificationKey),
      MnemonicSize(..),
      MnemonicToSigningKeyError(..),
      CommitteeColdExtendedKey,
      CommitteeColdKey,
      CommitteeHotExtendedKey,
      CommitteeHotKey,
      DRepExtendedKey,
      DRepKey,
      GenesisDelegateExtendedKey,
      GenesisDelegateKey,
      GenesisExtendedKey,
      GenesisKey,
      GenesisUTxOKey,
      PaymentExtendedKey,
      PaymentKey,
      StakeExtendedKey,
      StakeKey,
      AnyNewEpochState(..),
      ConditionResult(..),
      Env(..),
      FoldBlocksError(..),
      FoldStatus(..),
      GenesisConfig(..),
      GenesisConfigError(..),
      InitialLedgerStateError(..),
      LedgerState(..),
      LedgerStateError(..),
      NodeConfig(..),
      NodeConfigFile,
      ValidationMode(..),
      ChainDepStateProtocol,
      ConsensusBlockForEra,
      ConsensusModeParams(..),
      ConsensusProtocol,
      MonadIOTransError,
      MonadTransError,
      NetworkId(..),
      OperationalCertIssueError,
      OperationalCertificate,
      OperationalCertificateIssueCounter,
      DebugPlutusFailure(..),
      Ann,
      BlockType(..),
      Protocol(..),
      ProtocolInfoArgs(ProtocolInfoArgsByron, ProtocolInfoArgsShelley,
                       ProtocolInfoArgsCardano),
      SomeBlockType(..),
      CostModel(..),
      ExecutionUnitPrices(..),
      PraosNonce,
      ProtocolParametersConversionError(..),
      ProtocolParametersUpdate(..),
      UpdateProposal(..),
      EraHistory(..),
      LedgerEpochInfo(..),
      QueryInEra(..),
      QueryInMode(..),
      QueryInShelleyBasedEra(..),
      QueryUTxOFilter(..),
      SlotsInEpoch(..),
      SlotsToEpochEnd(..),
      DelegationsAndRewards(..),
      AnyPlutusScriptVersion(..),
      AnyScriptLanguage(..),
      ExecutionUnits(..),
      HasScriptLanguageInEra(..),
      IsPlutusScriptLanguage(..),
      IsScriptLanguage(..),
      IsScriptWitnessInCtx(..),
      KeyWitnessInCtx(..),
      PlutusScriptInEra(..),
      PlutusScriptV1,
      PlutusScriptV2,
      PlutusScriptV3,
      PlutusScriptVersion(..),
      ScriptDatum(..),
      ScriptHash(..),
      ScriptInAnyLang(..),
      ScriptLanguageInEra(..),
      ScriptRedeemer,
      ScriptWitnessInCtx(..),
      SimpleScript(..),
      SimpleScript',
      ToAlonzoScript(..),
      WitCtx(..),
      WitCtxMint,
      WitCtxStake,
      WitCtxTxIn,
      HashableScriptData,
      ScriptData(..),
      ScriptDataJsonBytesError(..),
      ScriptDataJsonError(..),
      ScriptDataJsonSchema(..),
      ScriptDataJsonSchemaError(..),
      ScriptDataRangeError(..),
      SerialiseAsCBOR(..),
      Bech32DecodeError(..),
      SerialiseAsBech32,
      JsonDecodeError(..),
      FromSomeTypeCDDL(..),
      TextEnvelopeCddlError(..),
      RawBytesHexError(..),
      SerialiseAsRawBytes(..),
      SerialiseAsRawBytesError(..),
      HasTextEnvelope(..),
      TextEnvelope(..),
      TextEnvelopeDescr,
      TextEnvelopeError(..),
      TextEnvelopeType(..),
      UsingBech32(..),
      UsingRawBytes(..),
      UsingRawBytesHex(..),
      StakePoolMetadata,
      StakePoolMetadataValidationError(..),
      AnyScriptWitness(..),
      ScriptWitnessIndex(..),
      TxBodyError(..),
      TxCertificates(..),
      TxProposalProcedures(..),
      TxReturnCollateral(..),
      TxTotalCollateral(..),
      TxUpdateProposal(..),
      TxVotingProcedures(..),
      TxWithdrawals(..),
      BuildTx,
      BuildTxWith(..),
      ViewTx,
      CtxTx,
      TxOutInAnyEra(..),
      TxOutValue(..),
      TxOutputError(..),
      ScriptValidity(..),
      ShelleyWitnessSigningKey(..),
      TxId(..),
      TxIx(..),
      AsTxMetadata(..),
      TxMetadata(..),
      TxMetadataJsonError(..),
      TxMetadataJsonSchema(..),
      TxMetadataJsonSchemaError(..),
      TxMetadataRangeError(..),
      TxMetadataValue(..),
      AssetId(..),
      AssetName(..),
      Lovelace,
      PolicyAssets(..),
      PolicyId(..),
      Quantity(..),
      ValueNestedBundle(..),
      ValueNestedRep(..),
      ShowOf(..),
      FromCBOR,
      ToCBOR,
      CommitteeMembersState(..),
      MemberStatus(..),
      EpochSlots(..),
      Inject(..),
      MIRPot(..),
      MIRTarget(..),
      BlockNo(..),
      EpochNo(..),
      SlotNo(..),
      SystemStart(..),
      MonadError(..),
      NodeToClientVersion(..),
      ChainSyncClient(..),
      ChainSyncClientPipelined(..),
      LocalStateQueryClient(..),
      LocalTxMonitorClient(..),
      MempoolSizeAndCapacity(..),
      LocalTxSubmissionClient(..),
      SubmitResult(..),
      Doc,
      Pretty(..),
      MonadTrans(..),
      Except,
      ExceptT(..) )
import Cardano.Api.Ledger as X (
  PParams,
 )
import Cardano.Api.Shelley as X (
  AcquiringFailure (..),
  Hash (HeaderHash),
  Key (..),
  PlutusScriptOrReferenceInput (PScript),
  PoolId,
  ShelleyGenesis (..),
  ShelleyLedgerEra,
  SigningKey (..),
  StakeCredential (..),
  VerificationKey (..),
  fromAlonzoCostModels,
  fromAlonzoPrices,
  fromPlutusData,
  fromShelleyMetadata,
  toAlonzoPrices,
  toPlutusData,
  toShelleyMetadata,
  toShelleyNetwork,
 )
import Cardano.Api.UTxO (
  UTxO,
  UTxO' (..),
 )
import Cardano.Ledger.Coin as X (Coin (..))
import Hydra.Cardano.Api.Prelude (
  Era,
  LedgerEra,
  LedgerProtocolParameters,
  Map,
  ledgerEraVersion,
 )

import Hydra.Cardano.Api.Address ()
import Hydra.Cardano.Api.AddressInEra as Extras
    ( fromLedgerAddr,
      fromPlutusAddress,
      mkScriptAddress,
      mkVkAddress,
      toLedgerAddr )
import Hydra.Cardano.Api.BlockHeader as Extras
    ( genBlockHeader, genBlockHeaderAt, genBlockHeaderHash )
import Hydra.Cardano.Api.ChainPoint as Extras
    ( genChainPoint, genChainPointAt, getChainPoint )
import Hydra.Cardano.Api.ExecutionUnits as Extras
    ( toLedgerExUnits )
import Hydra.Cardano.Api.Hash as Extras
    ( toLedgerKeyHash,
      toPlutusKeyHash,
      unsafeCastHash,
      unsafePaymentKeyHashFromBytes,
      unsafeScriptDataHashFromBytes )
import Hydra.Cardano.Api.NetworkId ()
import Hydra.Cardano.Api.NetworkMagic ()
import Hydra.Cardano.Api.PolicyId as Extras
    ( fromPlutusCurrencySymbol,
      toLedgerPolicyID,
      toPlutusCurrencySymbol )
import Hydra.Cardano.Api.ReferenceScript as Extras ( mkScriptRef )
import Hydra.Cardano.Api.ScriptData as Extras
    ( fromLedgerData,
      fromScriptData,
      toLedgerData,
      toScriptData,
      txOutScriptData,
      FromScriptData,
      ToScriptData )
import Hydra.Cardano.Api.ScriptDatum as Extras ( mkScriptDatum )
import Hydra.Cardano.Api.ScriptHash as Extras
    ( hashScriptInAnyLang )
import Hydra.Cardano.Api.StakeAddress as Extras
    ( mkScriptStakeAddress )
import Hydra.Cardano.Api.Tx as Extras
    ( fromLedgerTx,
      signTx,
      toLedgerTx,
      txSpendingUTxO,
      utxoProducedByTx )
import Hydra.Cardano.Api.TxBody as Extras
    ( findRedeemerSpending, lookupRedeemer )
import Hydra.Cardano.Api.TxId as Extras ( toLedgerTxId )
import Hydra.Cardano.Api.TxIn as Extras
    ( fromLedgerTxIn,
      fromPlutusTxOutRef,
      genTxIn,
      mkTxIn,
      toLedgerTxIn,
      toPlutusTxOutRef,
      txInputSet,
      txIns',
      withWitness )
import Hydra.Cardano.Api.TxOut as Extras
    ( findTxOutByAddress,
      findTxOutByScript,
      fromLedgerTxOut,
      fromPlutusTxOut,
      isScriptTxOut,
      isVkTxOut,
      mkTxOutAutoBalance,
      modifyTxOutAddress,
      modifyTxOutDatum,
      modifyTxOutValue,
      setMinUTxOValue,
      toLedgerTxOut,
      toPlutusTxOut,
      txOuts' )
import Hydra.Cardano.Api.TxOutDatum as Extras
    ( mkTxOutDatumHash, mkTxOutDatumInline )
import Hydra.Cardano.Api.TxOutValue as Extras ( mkTxOutValue )
import Hydra.Cardano.Api.UTxO as Extras
    ( fromLedgerUTxO,
      renderUTxO,
      resolveInputsUTxO,
      toLedgerUTxO,
      utxoFromTx )
import Hydra.Cardano.Api.ValidityInterval as Extras
    ( fromLedgerValidityInterval, toLedgerValidityInterval )
import Hydra.Cardano.Api.Value as Extras
    ( containsValue,
      fromLedgerMultiAsset,
      fromLedgerValue,
      fromPlutusValue,
      minUTxOValue,
      toLedgerValue,
      valueSize )
import Hydra.Cardano.Api.Witness as Extras
    ( mkScriptReference, mkScriptWitness )

import Cardano.Api qualified
import Cardano.Api.Internal.Tx.Body (TxInsReferenceDatums)
import Cardano.Api.Shelley qualified
import Cardano.Ledger.Alonzo.TxAuxData qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.BaseTypes as X (Network)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.ByteString.Short (ShortByteString)
import Prelude

-- ** AddressInEra

type AddressInEra = Cardano.Api.AddressInEra Era
{-# COMPLETE ShelleyAddressInEra, ByronAddressInEra #-}

pattern ShelleyAddressInEra :: Address ShelleyAddr -> AddressInEra
pattern ShelleyAddressInEra{address} <-
  Cardano.Api.AddressInEra Cardano.Api.ShelleyAddressInEra{} address
  where
    ShelleyAddressInEra =
      Cardano.Api.AddressInEra ShelleyAddressInAnyEra

pattern ByronAddressInEra :: Address ByronAddr -> AddressInEra
pattern ByronAddressInEra{byronAddress} <-
  Cardano.Api.AddressInEra Cardano.Api.ByronAddressInAnyEra byronAddress
  where
    ByronAddressInEra =
      Cardano.Api.AddressInEra ByronAddressInAnyEra

-- ** AddressTypeInEra

type AddressTypeInEra addrType = Cardano.Api.AddressTypeInEra addrType Era
{-# COMPLETE ByronAddressInAnyEra, ShelleyAddressInAnyEra #-}

pattern ByronAddressInAnyEra :: AddressTypeInEra ByronAddr
pattern ByronAddressInAnyEra <-
  Cardano.Api.ByronAddressInAnyEra
  where
    ByronAddressInAnyEra =
      Cardano.Api.ByronAddressInAnyEra

pattern ShelleyAddressInAnyEra :: AddressTypeInEra ShelleyAddr
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
{-# COMPLETE ShelleyBootstrapWitness, ShelleyKeyWitness #-}

pattern ShelleyBootstrapWitness :: Ledger.BootstrapWitness -> KeyWitness
pattern ShelleyBootstrapWitness{shelleyBootstrapWitness} <-
  Cardano.Api.Shelley.ShelleyBootstrapWitness _ shelleyBootstrapWitness
  where
    ShelleyBootstrapWitness =
      Cardano.Api.Shelley.ShelleyBootstrapWitness shelleyBasedEra

pattern ShelleyKeyWitness :: Ledger.WitVKey 'Ledger.Witness -> KeyWitness
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
{-# COMPLETE ShelleyTxBody #-}

pattern Tx :: TxBody -> [KeyWitness] -> Tx
pattern Tx{txBody, txKeyWitnesses} <-
  Cardano.Api.Tx txBody txKeyWitnesses
  where
    Tx =
      Cardano.Api.Tx

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
{-# COMPLETE TxAuxScriptsNone, TxAuxScripts #-}

pattern TxAuxScriptsNone :: TxAuxScripts
pattern TxAuxScriptsNone <-
  Cardano.Api.TxAuxScriptsNone
  where
    TxAuxScriptsNone =
      Cardano.Api.TxAuxScriptsNone

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

type TxBodyContent build = Cardano.Api.TxBodyContent build Era
{-# COMPLETE TxBodyContent #-}

pattern TxBodyContent ::
  TxIns build ->
  TxInsCollateral ->
  TxInsReference build ->
  [TxOut CtxTx] ->
  TxTotalCollateral Era ->
  TxReturnCollateral CtxTx Era ->
  TxFee ->
  TxValidityLowerBound ->
  TxValidityUpperBound ->
  TxMetadataInEra ->
  TxAuxScripts ->
  TxExtraKeyWitnesses ->
  BuildTxWith build (Maybe (LedgerProtocolParameters Era)) ->
  TxWithdrawals build Era ->
  TxCertificates build Era ->
  TxUpdateProposal Era ->
  TxMintValue build ->
  TxScriptValidity ->
  Maybe (Featured ConwayEraOnwards Era (TxProposalProcedures build Era)) ->
  Maybe (Featured ConwayEraOnwards Era (TxVotingProcedures build Era)) ->
  Maybe (Featured ConwayEraOnwards Era (Maybe Coin)) ->
  Maybe (Featured ConwayEraOnwards Era Coin) ->
  TxBodyContent build
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
{-# COMPLETE TxBodyNoScriptData, TxBodyScriptData #-}

pattern TxBodyNoScriptData :: TxBodyScriptData
pattern TxBodyNoScriptData <-
  Cardano.Api.TxBodyNoScriptData
  where
    TxBodyNoScriptData =
      Cardano.Api.TxBodyNoScriptData

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
{-# COMPLETE TxExtraKeyWitnessesNone, TxExtraKeyWitnesses #-}

pattern TxExtraKeyWitnessesNone :: TxExtraKeyWitnesses
pattern TxExtraKeyWitnessesNone <-
  Cardano.Api.TxExtraKeyWitnessesNone
  where
    TxExtraKeyWitnessesNone = Cardano.Api.TxExtraKeyWitnessesNone

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

type TxIns build = [(TxIn, BuildTxWith build (Cardano.Api.Witness WitCtxTxIn Era))]

-- ** TxInsReference

type TxInsReference build = Cardano.Api.TxInsReference build Era
{-# COMPLETE TxInsReferenceNone, TxInsReference #-}

pattern TxInsReferenceNone :: TxInsReference build
pattern TxInsReferenceNone <-
  Cardano.Api.TxInsReferenceNone
  where
    TxInsReferenceNone =
      Cardano.Api.TxInsReferenceNone

pattern TxInsReference :: [TxIn] -> TxInsReferenceDatums build -> TxInsReference build
pattern TxInsReference{txInsReference', scriptData} <-
  Cardano.Api.TxInsReference _ txInsReference' scriptData
  where
    TxInsReference =
      Cardano.Api.TxInsReference babbageBasedEra

-- ** TxInsCollateral

type TxInsCollateral = Cardano.Api.TxInsCollateral Era
{-# COMPLETE TxInsCollateralNone, TxInsCollateral #-}

pattern TxInsCollateralNone :: TxInsCollateral
pattern TxInsCollateralNone <-
  Cardano.Api.TxInsCollateralNone
  where
    TxInsCollateralNone =
      Cardano.Api.TxInsCollateralNone

pattern TxInsCollateral :: [TxIn] -> TxInsCollateral
pattern TxInsCollateral{txInsCollateral'} <-
  Cardano.Api.TxInsCollateral _ txInsCollateral'
  where
    TxInsCollateral =
      Cardano.Api.TxInsCollateral alonzoBasedEra

-- ** TxMetadataInEra

type TxMetadataInEra = Cardano.Api.TxMetadataInEra Era
{-# COMPLETE TxMetadataNone, TxMetadataInEra #-}

pattern TxMetadataNone :: TxMetadataInEra
pattern TxMetadataNone <-
  Cardano.Api.TxMetadataNone
  where
    TxMetadataNone =
      Cardano.Api.TxMetadataNone

pattern TxMetadataInEra :: TxMetadata -> TxMetadataInEra
pattern TxMetadataInEra{txMetadataInEra} <-
  Cardano.Api.TxMetadataInEra _ txMetadataInEra
  where
    TxMetadataInEra =
      Cardano.Api.TxMetadataInEra shelleyBasedEra

-- ** TxMintValue

type TxMintValue build = Cardano.Api.TxMintValue build Era
{-# COMPLETE TxMintValueNone, TxMintValue #-}

pattern TxMintValueNone :: TxMintValue build
pattern TxMintValueNone <-
  Cardano.Api.TxMintNone
  where
    TxMintValueNone =
      Cardano.Api.TxMintNone

pattern TxMintValue ::
  Map PolicyId (PolicyAssets, BuildTxWith build (ScriptWitness WitCtxMint)) ->
  TxMintValue build
pattern TxMintValue{txMintValueInEra} <-
  Cardano.Api.TxMintValue _ txMintValueInEra
  where
    TxMintValue =
      Cardano.Api.TxMintValue maryBasedEra
-- ** TxOut

type TxOut ctx = Cardano.Api.TxOut ctx Era
{-# COMPLETE TxOut #-}

-- | TxOut specialized for 'Era'
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
{-# COMPLETE ReferenceScript, ReferenceScriptNone #-}

pattern ReferenceScript :: ScriptInAnyLang -> ReferenceScript
pattern ReferenceScript{referenceScript} <-
  Cardano.Api.Shelley.ReferenceScript
    _
    referenceScript
  where
    ReferenceScript =
      Cardano.Api.Shelley.ReferenceScript
        babbageBasedEra

pattern ReferenceScriptNone :: Cardano.Api.Shelley.ReferenceScript Era
pattern ReferenceScriptNone <-
  Cardano.Api.Shelley.ReferenceScriptNone
  where
    ReferenceScriptNone =
      Cardano.Api.Shelley.ReferenceScriptNone

-- ** TxOutDatum

type TxOutDatum ctx = Cardano.Api.TxOutDatum ctx Era
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutSupplementalDatum, TxOutDatumInline #-}

pattern TxOutDatumNone :: TxOutDatum ctx
pattern TxOutDatumNone <-
  Cardano.Api.TxOutDatumNone
  where
    TxOutDatumNone =
      Cardano.Api.TxOutDatumNone

pattern TxOutDatumHash :: Hash ScriptData -> TxOutDatum ctx
pattern TxOutDatumHash{txOutDatumHash} <-
  Cardano.Api.TxOutDatumHash _ txOutDatumHash
  where
    TxOutDatumHash =
      Cardano.Api.TxOutDatumHash alonzoBasedEra

pattern TxOutSupplementalDatum :: HashableScriptData -> TxOutDatum CtxTx
pattern TxOutSupplementalDatum{txOutDatumScriptData} <-
  Cardano.Api.TxOutSupplementalDatum _ txOutDatumScriptData
  where
    TxOutSupplementalDatum =
      Cardano.Api.TxOutSupplementalDatum alonzoBasedEra

pattern TxOutDatumInline :: HashableScriptData -> TxOutDatum ctx
pattern TxOutDatumInline{txOutDatumInlineScriptData} <-
  Cardano.Api.TxOutDatumInline _ txOutDatumInlineScriptData
  where
    TxOutDatumInline =
      Cardano.Api.TxOutDatumInline babbageBasedEra

-- ** TxScriptValidity

type TxScriptValidity = Cardano.Api.TxScriptValidity Era
{-# COMPLETE TxScriptValidityNone, TxScriptValidity #-}

pattern TxScriptValidityNone :: TxScriptValidity
pattern TxScriptValidityNone <-
  Cardano.Api.TxScriptValidityNone
  where
    TxScriptValidityNone =
      Cardano.Api.TxScriptValidityNone

pattern TxScriptValidity :: ScriptValidity -> TxScriptValidity
pattern TxScriptValidity{txScriptValidity'} <-
  Cardano.Api.TxScriptValidity _ txScriptValidity'
  where
    TxScriptValidity =
      Cardano.Api.TxScriptValidity alonzoBasedEra

-- ** TxValidityLowerBound

type TxValidityLowerBound = Cardano.Api.TxValidityLowerBound Era
{-# COMPLETE TxValidityNoLowerBound, TxValidityLowerBound #-}

pattern TxValidityNoLowerBound :: TxValidityLowerBound
pattern TxValidityNoLowerBound <-
  Cardano.Api.TxValidityNoLowerBound
  where
    TxValidityNoLowerBound =
      Cardano.Api.TxValidityNoLowerBound

pattern TxValidityLowerBound :: SlotNo -> TxValidityLowerBound
pattern TxValidityLowerBound{lowerBound} <-
  Cardano.Api.TxValidityLowerBound _ lowerBound
  where
    TxValidityLowerBound =
      Cardano.Api.TxValidityLowerBound allegraBasedEra

-- ** TxValidityUpperBound

type TxValidityUpperBound = Cardano.Api.TxValidityUpperBound Era
{-# COMPLETE TxValidityNoUpperBound, TxValidityUpperBound #-}

pattern TxValidityNoUpperBound :: TxValidityUpperBound
pattern TxValidityNoUpperBound <-
  Cardano.Api.TxValidityUpperBound _ Nothing
  where
    TxValidityNoUpperBound =
      Cardano.Api.TxValidityUpperBound shelleyBasedEra Nothing

pattern TxValidityUpperBound :: SlotNo -> TxValidityUpperBound
pattern TxValidityUpperBound{upperBound} <-
  Cardano.Api.TxValidityUpperBound _ (Just upperBound)
  where
    TxValidityUpperBound =
      Cardano.Api.TxValidityUpperBound shelleyBasedEra . Just

-- ** Witness

type Witness witCtx = Cardano.Api.Witness witCtx Era
{-# COMPLETE ScriptWitness, KeyWitness #-}

pattern KeyWitness :: KeyWitnessInCtx ctx -> Witness ctx
pattern KeyWitness keyWitnessInCtx <-
  Cardano.Api.KeyWitness keyWitnessInCtx
  where
    KeyWitness = Cardano.Api.KeyWitness

pattern ScriptWitness :: ScriptWitnessInCtx ctx -> ScriptWitness ctx -> Witness ctx
pattern ScriptWitness scriptWitnessInCtx scriptWitness <-
  Cardano.Api.ScriptWitness scriptWitnessInCtx scriptWitness
  where
    ScriptWitness = Cardano.Api.ScriptWitness

makeShelleyKeyWitness :: TxBody -> ShelleyWitnessSigningKey -> KeyWitness
makeShelleyKeyWitness = Cardano.Api.makeShelleyKeyWitness shelleyBasedEra
