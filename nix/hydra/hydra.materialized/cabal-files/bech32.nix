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
    flags = { release = false; static = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bech32"; version = "1.1.2"; };
      license = "Apache-2.0";
      copyright = "2017 Marko Bencun, 2019-2020 IOHK";
      maintainer = "operations@iohk.io, erikd@mega-nerd.com, jonathan.knowles@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/bech32";
      url = "";
      synopsis = "Implementation of the Bech32 cryptocurrency address format (BIP 0173).";
      description = "Implementation of the Bech32 cryptocurrency address format documented in the\nBIP (Bitcoin Improvement Proposal) 0173.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      exes = {
        "bech32" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "bech32-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.buildPackages.bech32.components.exes.bech32 or (pkgs.buildPackages.bech32 or (errorHandler.buildToolDepError "bech32:bech32")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bech32-1.1.2.tar.gz";
      sha256 = "489e1922dce97ce59f72d87b17480eb0087b6661d4bcb7be124e027abcb7d2c7";
      });
    }) // {
    package-description-override = "name:          bech32\nversion:       1.1.2\nsynopsis:      Implementation of the Bech32 cryptocurrency address format (BIP 0173).\ndescription:   Implementation of the Bech32 cryptocurrency address format documented in the\n               BIP (Bitcoin Improvement Proposal) 0173.\nauthor:        IOHK Engineering Team\nmaintainer:    operations@iohk.io, erikd@mega-nerd.com, jonathan.knowles@iohk.io\ncopyright:     2017 Marko Bencun, 2019-2020 IOHK\nlicense:       Apache-2.0\nlicense-file:  LICENSE\nhomepage:      https://github.com/input-output-hk/bech32\nbug-reports:   https://github.com/input-output-hk/bech32/issues\ncategory:      Web\nbuild-type:    Simple\nextra-source-files:  ChangeLog.md\ncabal-version: >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/bech32.git\n\nflag release\n  description: Strict compiler warning checks.\n  default: False\n  manual: True\n\nflag static\n  description: Try to build a static executable.\n  default: False\n  manual: True\n\nlibrary\n  default-language:\n      Haskell2010\n  default-extensions:\n      NoImplicitPrelude\n      OverloadedStrings\n  ghc-options:\n      -Wall -Wcompat -fwarn-redundant-constraints\n  if flag(release)\n    ghc-options: -Werror\n  build-depends:\n      array\n    , base >= 4.11.1.0 && <5\n    , bytestring\n    , containers\n    , extra\n    , text\n  hs-source-dirs:\n      src\n  exposed-modules:\n      Codec.Binary.Bech32\n      Codec.Binary.Bech32.Internal\n\nexecutable bech32\n  main-is: Main.hs\n  other-modules:\n      Paths_bech32\n  hs-source-dirs:\n      app\n  build-depends:\n      base\n    , base58-bytestring\n    , bech32\n    , bytestring\n    , extra\n    , memory\n    , optparse-applicative\n    , text\n  ghc-options:\n      -Wall -Wcompat -fwarn-redundant-constraints\n      -threaded -rtsopts -with-rtsopts=-N\n  if flag(release)\n    ghc-options: -Werror\n  if flag(static)\n    ghc-options: -static\n    cc-options: -static\n    ld-options: -static -pthread\n  default-language: Haskell2010\n\ntest-suite bech32-test\n  default-language:\n      Haskell2010\n  type:\n      exitcode-stdio-1.0\n  hs-source-dirs:\n      test\n  ghc-options:\n      -Wall\n      -threaded -rtsopts -with-rtsopts=-N\n  if flag(release)\n    ghc-options: -Werror\n  build-depends:\n      base\n    , base58-bytestring\n    , bech32\n    , bytestring\n    , containers\n    , deepseq\n    , extra\n    , hspec\n    , memory\n    , process\n    , QuickCheck >= 2.12\n    , text\n    , vector\n  build-tools:\n      hspec-discover\n    , bech32\n  main-is:\n      Main.hs\n  other-modules:\n      AppSpec\n      Codec.Binary.Bech32Spec\n";
    }