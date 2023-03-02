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
    flags = { release = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bech32-th"; version = "1.1.1"; };
      license = "Apache-2.0";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io, erikd@mega-nerd.com, jonathan.knowles@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/bech32";
      url = "";
      synopsis = "Template Haskell extensions to the Bech32 library.";
      description = "Template Haskell extensions to the Bech32 library, including\nquasi-quoters for compile-time checking of Bech32 string\nliterals.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "bech32-th-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bech32-th-1.1.1.tar.gz";
      sha256 = "3346a2539bfafd2e4cb8ca775a0c4aeb5636f52af4a9b0a8c22478b393558814";
      });
    }) // {
    package-description-override = "name:               bech32-th\nversion:            1.1.1\nsynopsis:           Template Haskell extensions to the Bech32 library.\ndescription:        Template Haskell extensions to the Bech32 library, including\n                    quasi-quoters for compile-time checking of Bech32 string\n                    literals.\nauthor:             IOHK Engineering Team\nmaintainer:         operations@iohk.io, erikd@mega-nerd.com, jonathan.knowles@iohk.io\ncopyright:          2020 IOHK\nlicense:            Apache-2.0\nlicense-file:       LICENSE\nhomepage:           https://github.com/input-output-hk/bech32\nbug-reports:        https://github.com/input-output-hk/bech32/issues\ncategory:           Web\nbuild-type:         Simple\nextra-source-files: ChangeLog.md\ncabal-version:      >=1.10\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/bech32.git\n\nflag release\n  description: Strict compiler warning checks.\n  default: False\n  manual: True\n\nlibrary\n  default-language:\n      Haskell2010\n  default-extensions:\n      NoImplicitPrelude\n      OverloadedStrings\n  ghc-options:\n      -Wall -Wcompat -fwarn-redundant-constraints\n  if flag(release)\n    ghc-options: -Werror\n  build-depends:\n      base >= 4.11.1.0 && < 5\n    , bech32 >= 1.1.1\n    , template-haskell\n    , text\n  hs-source-dirs:\n      src\n  exposed-modules:\n      Codec.Binary.Bech32.TH\n\ntest-suite bech32-th-test\n  default-language:\n      Haskell2010\n  default-extensions:\n      NoImplicitPrelude\n      OverloadedStrings\n  type:\n      exitcode-stdio-1.0\n  hs-source-dirs:\n      test\n  ghc-options:\n      -Wall\n      -threaded -rtsopts -with-rtsopts=-N\n  if flag(release)\n    ghc-options: -Werror\n  build-depends:\n      base\n    , bech32\n    , bech32-th\n    , hspec\n    , template-haskell\n  build-tools:\n      hspec-discover\n  main-is:\n      Main.hs\n  other-modules:\n      Codec.Binary.Bech32.THSpec\n";
    }