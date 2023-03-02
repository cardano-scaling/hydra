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
    flags = { golden-tests = false; golden-tests-exe = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-crypto"; version = "1.1.1"; };
      license = "MIT";
      copyright = "2016-2021 IOHK";
      maintainer = "contact@typed.io";
      author = "Vincent Hanquez";
      homepage = "https://github.com/input-output-hk/cardano-crypto#readme";
      url = "";
      synopsis = "Cryptography primitives for cardano";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          ];
        buildable = true;
        };
      exes = {
        "golden-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            ] ++ (pkgs.lib).optional (flags.golden-tests-exe) (hsPkgs."inspector" or (errorHandler.buildDepError "inspector"));
          buildable = if flags.golden-tests-exe then true else false;
          };
        };
      tests = {
        "cardano-crypto-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            ];
          buildable = true;
          };
        "cardano-crypto-golden-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
            (hsPkgs."foundation" or (errorHandler.buildDepError "foundation"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            ] ++ (pkgs.lib).optional (flags.golden-tests) (hsPkgs."inspector" or (errorHandler.buildDepError "inspector"));
          buildable = if flags.golden-tests then true else false;
          };
        };
      benchmarks = {
        "cardano-crypto-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-crypto-1.1.1.tar.gz";
      sha256 = "1eca0cf717c1a6fae6e45864c3135c58a128c7b2157c324effe49baabcbf979b";
      });
    }) // {
    package-description-override = "name:                cardano-crypto\nversion:             1.1.1\nsynopsis:            Cryptography primitives for cardano\ndescription:\nhomepage:            https://github.com/input-output-hk/cardano-crypto#readme\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Vincent Hanquez\nmaintainer:          contact@typed.io\ncopyright:           2016-2021 IOHK\ncategory:            Crypto\nbuild-type:          Simple\nextra-source-files:  README.md\n                     cbits/*.h\n                     cbits/ed25519/*.h\ncabal-version:       >=1.10\n\nflag golden-tests\n  description:       compile the golden tests and run them\n  default:           False\n  manual:            True\n\nflag golden-tests-exe\n  description:       compile the golden test binary allowing to generate pretty markdown output\n  default:           False\n  manual:            True\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Cardano.Crypto.Wallet\n                       Cardano.Crypto.Wallet.Encrypted\n                       Cardano.Crypto.Wallet.Types\n                       Cardano.Crypto.Wallet.Pure\n                       Cardano.Crypto.Encoding.BIP39\n                       Cardano.Crypto.Encoding.Seed\n                       Crypto.Math.Edwards25519\n                       Crypto.Math.Bits\n                       Crypto.Math.Bytes\n                       Crypto.Math.NatMath\n                       Crypto.ECC.Ed25519Donna\n                       Crypto.ECC.Ed25519BIP32\n                       Crypto.Encoding.BIP39\n                       Crypto.Encoding.BIP39.Dictionary\n                       Crypto.Encoding.BIP39.English\n                       Cardano.Internal.Compat\n  build-depends:       base >= 4.7 && < 5\n                     , memory\n                     , deepseq\n                     , bytestring\n                     , basement\n                     , foundation\n                     , cryptonite >= 0.22\n                     , hashable\n                     , integer-gmp\n  default-language:    Haskell2010\n  C-sources:           cbits/ed25519/ed25519.c\n                       cbits/encrypted_sign.c\n  include-dirs:        cbits/ed25519 cbits\n  default-extensions:  GeneralizedNewtypeDeriving\n  ghc-options:         -Wall\n  cc-options:          -Wall -Wno-unused-function\n  if impl(ghc >= 8.6)\n    default-extensions: NoStarIsType\n\ntest-suite cardano-crypto-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  other-modules:       Test.Crypto\n                       Test.Crypto.Encoding\n                       Test.Crypto.Encoding.BIP39\n                       Test.Cardano\n                       Test.Cardano.Crypto\n                       Test.Cardano.Crypto.Encoding\n                       Test.Cardano.Crypto.Encoding.Seed\n                       Utils\n  build-depends:       base\n                     , bytestring\n                     , memory\n                     , cryptonite\n                     , cardano-crypto\n                     , basement\n                     , foundation\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n\ntest-suite cardano-crypto-golden-tests\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             GoldenTest.hs\n  other-modules:       Test.Orphans\n  build-depends:       base\n                     , basement\n                     , foundation\n                     , memory\n                     , bytestring\n                     , cryptonite\n                     , cardano-crypto\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-extensions:  NoImplicitPrelude\n  default-language:    Haskell2010\n  if flag(golden-tests)\n    build-depends:     inspector\n    buildable:         True\n  else\n    buildable:         False\n\nexecutable golden-tests\n  hs-source-dirs:      test\n  main-is:             GoldenTest.hs\n  other-modules:       Test.Orphans\n  if flag(golden-tests-exe)\n    build-depends:     inspector\n    buildable:         True\n  else\n    buildable:         False\n  build-depends:       base\n                     , basement\n                     , foundation\n                     , memory\n                     , bytestring\n                     , cryptonite\n                     , cardano-crypto\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-extensions:  NoImplicitPrelude\n  default-language:    Haskell2010\n\nbenchmark cardano-crypto-bench\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      benchs\n  Main-is:             Bench.hs\n  build-depends:       base\n                     , bytestring\n                     , memory\n                     , cryptonite\n                     , cardano-crypto\n                     , gauge\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/cardano-crypto\n";
    }