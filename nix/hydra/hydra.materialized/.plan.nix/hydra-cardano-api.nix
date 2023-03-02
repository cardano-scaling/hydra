{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { hydra-development = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "hydra-cardano-api"; version = "0.9.0"; };
      license = "Apache-2.0";
      copyright = "2022 IOG";
      maintainer = "";
      author = "IOG";
      homepage = "";
      url = "";
      synopsis = "A Haskell API for Cardano, tailored to the Hydra project.";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [
          "Cardano/Api/UTxO"
          "Hydra/Cardano/Api"
          "Hydra/Cardano/Api/Address"
          "Hydra/Cardano/Api/AddressInEra"
          "Hydra/Cardano/Api/ChainPoint"
          "Hydra/Cardano/Api/CtxTx"
          "Hydra/Cardano/Api/CtxUTxO"
          "Hydra/Cardano/Api/ExecutionUnits"
          "Hydra/Cardano/Api/Hash"
          "Hydra/Cardano/Api/KeyWitness"
          "Hydra/Cardano/Api/Lovelace"
          "Hydra/Cardano/Api/MultiAssetSupportedInEra"
          "Hydra/Cardano/Api/Network"
          "Hydra/Cardano/Api/NetworkId"
          "Hydra/Cardano/Api/PlutusScript"
          "Hydra/Cardano/Api/PlutusScriptVersion"
          "Hydra/Cardano/Api/PolicyId"
          "Hydra/Cardano/Api/Prelude"
          "Hydra/Cardano/Api/Pretty"
          "Hydra/Cardano/Api/ReferenceScript"
          "Hydra/Cardano/Api/ReferenceTxInsScriptsInlineDatumsSupportedInEra"
          "Hydra/Cardano/Api/ScriptData"
          "Hydra/Cardano/Api/ScriptDataSupportedInEra"
          "Hydra/Cardano/Api/ScriptDatum"
          "Hydra/Cardano/Api/ScriptHash"
          "Hydra/Cardano/Api/ScriptLanguageInEra"
          "Hydra/Cardano/Api/ScriptWitnessInCtx"
          "Hydra/Cardano/Api/Tx"
          "Hydra/Cardano/Api/TxBody"
          "Hydra/Cardano/Api/TxId"
          "Hydra/Cardano/Api/TxIn"
          "Hydra/Cardano/Api/TxOut"
          "Hydra/Cardano/Api/TxOutDatum"
          "Hydra/Cardano/Api/TxOutValue"
          "Hydra/Cardano/Api/TxScriptValidity"
          "Hydra/Cardano/Api/UsingRawBytesHex"
          "Hydra/Cardano/Api/UTxO"
          "Hydra/Cardano/Api/ValidityInterval"
          "Hydra/Cardano/Api/Value"
          "Hydra/Cardano/Api/VerificationKey"
          "Hydra/Cardano/Api/Witness"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../hydra-cardano-api; }