{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = {
        name = "cardano-ledger-shelley-ma-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Shelley ledger with multiasset and time lock support.";
      description = "This package extends the Shelley ledger with support for\nnative tokens and timelocks.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."generic-random" or (errorHandler.buildDepError "generic-random"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "cardano-ledger-shelley-ma-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-ledger-shelley-ma-test-0.1.0.0.tar.gz";
      sha256 = "76fe644b4ded4a91224abec01ed57cc2d02f509bb945761895bf47fd2702ea90";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-ledger-shelley-ma-test\nversion:             0.1.0.0\nsynopsis:            Shelley ledger with multiasset and time lock support.\ndescription:\n  This package extends the Shelley ledger with support for\n  native tokens and timelocks.\nbug-reports:         https://github.com/input-output-hk/cardano-ledger/issues\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ncopyright:           2020 Input Output (Hong Kong) Ltd.\ncategory:            Network\nbuild-type:          Simple\n\nextra-source-files:\n  cddl-files/shelley-ma.cddl\n  cddl-files/real/crypto.cddl\n  cddl-files/mock/extras.cddl\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-ledger\n  subdir:   eras/shelley-ma/test-suite\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Werror\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wpartial-fields\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:\n    Test.Cardano.Ledger.TranslationTools\n    Test.Cardano.Ledger.EraBuffet\n    Test.Cardano.Ledger.MaryEraGen\n    Test.Cardano.Ledger.Mary.Golden\n    Test.Cardano.Ledger.Mary.Examples.Consensus\n    Test.Cardano.Ledger.AllegraEraGen\n    Test.Cardano.Ledger.Allegra.Examples.Consensus\n    Test.Cardano.Ledger.ShelleyMA.TxBody\n    Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators\n    Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip\n\n  build-depends:\n    base16-bytestring,\n    bytestring,\n    cardano-binary,\n    cardano-data,\n    cardano-crypto-class,\n    cardano-ledger-core,\n    cardano-ledger-pretty,\n    cardano-ledger-shelley-ma,\n    cardano-slotting,\n    cborg,\n    containers,\n    generic-random,\n    hashable,\n    mtl,\n    QuickCheck >= 2.13.2,\n    cardano-ledger-shelley-test,\n    cardano-ledger-shelley,\n    strict-containers,\n    tasty-hunit,\n    tasty-quickcheck,\n    tasty,\n    text,\n  hs-source-dirs: src\n\ntest-suite cardano-ledger-shelley-ma-test\n  import:             base, project-config\n\n  type:               exitcode-stdio-1.0\n  main-is:            Tests.hs\n  hs-source-dirs:     test\n  other-modules:\n      Test.Cardano.Ledger.Mary.Examples\n      Test.Cardano.Ledger.Mary.Examples.Cast\n      Test.Cardano.Ledger.Mary.Examples.MultiAssets\n      Test.Cardano.Ledger.Mary.Translation\n      Test.Cardano.Ledger.Mary.Value\n      Test.Cardano.Ledger.Allegra.Translation\n      Test.Cardano.Ledger.Allegra.ScriptTranslation\n      Test.Cardano.Ledger.ShelleyMA.Serialisation\n      Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL\n      Test.Cardano.Ledger.ShelleyMA.Serialisation.Golden.Encoding\n      Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks\n  -- We set a bound here so that we're alerted of potential space\n  -- leaks in our generators (or test) code.\n  --\n  -- The 4 megabytes stack bound and 250 megabytes heap bound were\n  -- determined ad-hoc.\n  ghc-options:        -threaded\n                      -rtsopts\n                      -with-rtsopts=-N\n                      \"-with-rtsopts=-K4m -M250m\"\n  build-depends:\n      bytestring,\n      cardano-binary,\n      cardano-data,\n      cardano-ledger-core,\n      cardano-ledger-shelley-ma-test,\n      cardano-ledger-shelley-ma,\n      cardano-protocol-tpraos,\n      cardano-slotting,\n      cborg,\n      containers,\n      data-default-class,\n      mtl,\n      QuickCheck,\n      cardano-ledger-shelley-test,\n      cardano-ledger-shelley,\n      small-steps-test,\n      small-steps,\n      strict-containers,\n      tasty-hunit,\n      tasty-quickcheck,\n      tasty,\n";
    }