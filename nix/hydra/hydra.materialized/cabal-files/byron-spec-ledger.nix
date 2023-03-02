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
      identifier = { name = "byron-spec-ledger"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-legder-specs";
      url = "";
      synopsis = "Executable specification of Cardano ledger";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."goblins" or (errorHandler.buildDepError "goblins"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          ];
        buildable = true;
        };
      tests = {
        "byron-spec-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."Unique" or (errorHandler.buildDepError "Unique"))
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
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/byron-spec-ledger-0.1.0.0.tar.gz";
      sha256 = "08eeee005cb3933857967ad0cf03a7036479eadcd6423c40f6840198496f52be";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                byron-spec-ledger\nversion:             0.1.0.0\nsynopsis:            Executable specification of Cardano ledger\n-- description:\nhomepage:            https://github.com/input-output-hk/cardano-legder-specs\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\n-- copyright:\ncategory:            Testing\nbuild-type:          Simple\n\nextra-source-files:  CHANGELOG.md\n                     src/goblin_genomes/*.genome\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.15\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wno-unused-packages\n\nlibrary\n  import:             base, project-config\n\n  hs-source-dirs:      src\n  exposed-modules:     Hedgehog.Gen.Double\n                     , Byron.Spec.Ledger.Core\n                     , Byron.Spec.Ledger.Core.Generators\n                     , Byron.Spec.Ledger.Core.Omniscient\n                     , Byron.Spec.Ledger.Delegation\n                     , Byron.Spec.Ledger.Delegation.Test\n                     , Byron.Spec.Ledger.GlobalParams\n                     , Byron.Spec.Ledger.Update\n                     , Byron.Spec.Ledger.Update.Generators\n                     , Byron.Spec.Ledger.Update.Test\n                     , Byron.Spec.Ledger.UTxO\n                     , Byron.Spec.Ledger.UTxO.Generators\n                     , Byron.Spec.Ledger.Util\n                     , Byron.Spec.Ledger.STS.UTXO\n                     , Byron.Spec.Ledger.STS.UTXOW\n                     , Byron.Spec.Ledger.STS.UTXOWS\n  build-depends:       bimap >=0.4 && <0.5\n                     , cardano-data\n                     , containers\n                     , filepath\n                     , goblins\n                     , hashable\n                     , hedgehog >= 1.0.4\n                     , microlens\n                     , microlens-th\n                     , nothunks\n                     , template-haskell\n                     , Unique >= 0.4.7.6\n                     -- IOHK deps\n                     , cardano-binary\n                     -- Local deps\n                     , small-steps\n                     , small-steps-test\n\ntest-suite byron-spec-ledger-test\n  import:             base, project-config\n\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules: Test.Byron.Spec.Ledger.Core.Generators.Properties\n               , Test.Byron.Spec.Ledger.Delegation.Examples\n               , Test.Byron.Spec.Ledger.Delegation.Properties\n               , Test.Byron.Spec.Ledger.AbstractSize.Properties\n               , Test.Byron.Spec.Ledger.Update.Examples\n               , Test.Byron.Spec.Ledger.Update.Properties\n               , Test.Byron.Spec.Ledger.Relation.Properties\n               , Test.Byron.Spec.Ledger.UTxO.Properties\n  type: exitcode-stdio-1.0\n  default-language:    Haskell2010\n  build-depends: bimap >=0.4 && <0.5\n               , cardano-data\n               , containers\n               , hedgehog >= 1.0.4\n               , microlens\n               , microlens-th\n               , tasty\n               , tasty-hunit\n               , tasty-hedgehog\n               , Unique >= 0.4.7.6\n               -- Local deps\n               , byron-spec-ledger\n               , small-steps\n               , small-steps-test\n\n  -- We set a bound here so that we're alerted of potential space\n  -- leaks in our generators (or test) code.\n  --\n  -- The 4 megabytes stack bound and 150 megabytes heap bound were\n  -- determined ad-hoc.\n  ghc-options:        \"-with-rtsopts=-K4m -M150m\"\n";
    }