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
    flags = { lib-werror = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "string-conv"; version = "0.2.0"; };
      license = "BSD-3-Clause";
      copyright = "Soostone Inc, 2012-2015";
      maintainer = "ozgun.ataman@soostone.com";
      author = "Ozgun Ataman";
      homepage = "https://github.com/Soostone/string-conv";
      url = "";
      synopsis = "Standardized conversion between string types";
      description = "Avoids the need to remember many different functions for converting string types. Just use one universal function toS for all monomorphic string conversions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."string-conv" or (errorHandler.buildDepError "string-conv"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/string-conv-0.2.0.tar.gz";
      sha256 = "39cea3010eb1c52b6dd21c4108e23b89926f0f21b872ad1f5f644328c73a9096";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.6.\n--\n-- see: https://github.com/sol/hpack\n\nname:           string-conv\nversion:        0.2.0\nsynopsis:       Standardized conversion between string types\ndescription:    Avoids the need to remember many different functions for converting string types. Just use one universal function toS for all monomorphic string conversions.\ncategory:       Data, String, Text\nhomepage:       https://github.com/Soostone/string-conv\nbug-reports:    https://github.com/Soostone/string-conv/issues\nauthor:         Ozgun Ataman\nmaintainer:     ozgun.ataman@soostone.com\ncopyright:      Soostone Inc, 2012-2015\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    changelog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/Soostone/string-conv\n\nflag lib-Werror\n  description: Turn on -Wall and -Werror. Should always be enabled in development.\n\n  manual: True\n  default: False\n\nlibrary\n  exposed-modules:\n      Data.String.Conv\n  other-modules:\n      Paths_string_conv\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.4 && <5\n    , bytestring\n    , text\n  if flag(lib-Werror)\n    ghc-options: -Wall -Werror -fwarn-redundant-constraints -Wincomplete-record-updates -Wincomplete-uni-patterns -Widentities\n  else\n    ghc-options: -Wall -fwarn-redundant-constraints -Wincomplete-record-updates -Widentities\n  default-language: Haskell2010\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n      Paths_string_conv\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.4 && <5\n    , bytestring\n    , quickcheck-instances >=0.3.17\n    , string-conv\n    , tasty\n    , tasty-quickcheck\n    , text\n  if flag(lib-Werror)\n    ghc-options: -Wall -Werror -fwarn-redundant-constraints -Wincomplete-record-updates -Wincomplete-uni-patterns -Widentities\n  else\n    ghc-options: -Wall -fwarn-redundant-constraints -Wincomplete-record-updates -Widentities\n  default-language: Haskell2010\n";
    }