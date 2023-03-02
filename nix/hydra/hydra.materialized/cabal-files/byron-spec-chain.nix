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
      identifier = { name = "byron-spec-chain"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Executable specification of the Cardano blockchain";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
          (hsPkgs."goblins" or (errorHandler.buildDepError "goblins"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          ];
        buildable = true;
        };
      tests = {
        "chain-rules-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."data-ordlist" or (errorHandler.buildDepError "data-ordlist"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."byron-spec-chain" or (errorHandler.buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (errorHandler.buildDepError "byron-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/byron-spec-chain-0.1.0.0.tar.gz";
      sha256 = "461b2345237ae06fa4660f277d6ad7a56c486a8fb362f15e179d4f07594d0eeb";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                byron-spec-chain\nversion:             0.1.0.0\nsynopsis:            Executable specification of the Cardano blockchain\n-- description:\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\n-- copyright:\ncategory:            Testing\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n\n  exposed-modules:     Byron.Spec.Chain.STS.Block\n                     , Byron.Spec.Chain.STS.Rule.BBody\n                     , Byron.Spec.Chain.STS.Rule.Bupi\n                     , Byron.Spec.Chain.STS.Rule.Chain\n                     , Byron.Spec.Chain.STS.Rule.Epoch\n                     , Byron.Spec.Chain.STS.Rule.Pbft\n                     , Byron.Spec.Chain.STS.Rule.SigCnt\n\n  hs-source-dirs:      src\n  build-depends:       bimap >=0.4 && <0.5\n                     , bytestring\n                     , cardano-data\n                     , containers\n                     , byron-spec-ledger\n                     , goblins\n                     , hashable\n                     , hedgehog >= 1.0.4\n                     , microlens\n                     , microlens-th\n                     , small-steps\n                     , small-steps-test\n\ntest-suite chain-rules-test\n  import:             base, project-config\n\n  hs-source-dirs:      test\n  main-is:             Main.hs\n  other-modules:       Test.Byron.Spec.Chain.STS.Properties\n                     , Test.Byron.AbstractSize.Properties\n  type:                exitcode-stdio-1.0\n  default-language:    Haskell2010\n  build-depends:       containers\n                     , cardano-data\n                     , data-ordlist\n                     , hedgehog >= 1.0.4\n                     , microlens\n                     , tasty\n                     , tasty-hedgehog\n                     , tasty-hunit\n                     -- local deps\n                     , byron-spec-chain\n                     , byron-spec-ledger\n                     , small-steps\n                     , small-steps-test\n\n  -- See `byron-spec-ledger.cabal` for an explanation of the options below.\n  ghc-options:         \"-with-rtsopts=-K4m -M300m\"\n";
    }