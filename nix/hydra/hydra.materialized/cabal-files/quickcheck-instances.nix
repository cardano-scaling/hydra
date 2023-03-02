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
    flags = { bytestring-builder = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "quickcheck-instances"; version = "0.3.28"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2012-2016 Antoine Latter, 2017-2019 Oleg Grenrus";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Antoine Latter <aslatter@gmail.com>, Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/haskellari/qc-instances";
      url = "";
      synopsis = "Common quickcheck instances";
      description = "QuickCheck instances.\n\nThe goal is to supply QuickCheck instances for\ntypes provided by the Haskell Platform.\n\nSince all of these instances are provided as\norphans, I recommend that you do not use this library\nwithin another library module, so that you don't\nimpose these instances on down-stream consumers of\nyour code.\n\nFor information on writing a test-suite with Cabal\nsee <https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."integer-logarithms" or (errorHandler.buildDepError "integer-logarithms"))
          (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
          (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (if flags.bytestring-builder
          then [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-builder" or (errorHandler.buildDepError "bytestring-builder"))
            ]
          else [
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.8") (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"));
        buildable = true;
        };
      tests = {
        "self-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bytestring-gen" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/quickcheck-instances-0.3.28.tar.gz";
      sha256 = "c4111b7b49d5814120cb6055705d04eda7dd32a536057167c646aa77b68ccccb";
      });
    }) // {
    package-description-override = "name:               quickcheck-instances\nversion:            0.3.28\nsynopsis:           Common quickcheck instances\ndescription:\n  QuickCheck instances.\n  .\n  The goal is to supply QuickCheck instances for\n  types provided by the Haskell Platform.\n  .\n  Since all of these instances are provided as\n  orphans, I recommend that you do not use this library\n  within another library module, so that you don't\n  impose these instances on down-stream consumers of\n  your code.\n  .\n  For information on writing a test-suite with Cabal\n  see <https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites>\n\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:\n  Antoine Latter <aslatter@gmail.com>, Oleg Grenrus <oleg.grenrus@iki.fi>\n\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nhomepage:           https://github.com/haskellari/qc-instances\nbug-reports:        https://github.com/haskellari/qc-instances/issues\ncopyright:          Copyright 2012-2016 Antoine Latter, 2017-2019 Oleg Grenrus\ncategory:           Testing\nbuild-type:         Simple\nextra-source-files: CHANGES\ncabal-version:      >=1.10\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/qc-instances.git\n\nflag bytestring-builder\n  description: Use bytestring-builder (with old bytestring)\n  default:     False\n  manual:      False\n\nlibrary\n  default-language: Haskell2010\n  exposed-modules:\n    Test.QuickCheck.Instances\n    Test.QuickCheck.Instances.Array\n    Test.QuickCheck.Instances.Array.Byte\n    Test.QuickCheck.Instances.ByteString\n    Test.QuickCheck.Instances.CaseInsensitive\n    Test.QuickCheck.Instances.Containers\n    Test.QuickCheck.Instances.DataFix\n    Test.QuickCheck.Instances.Hashable\n    Test.QuickCheck.Instances.Natural\n    Test.QuickCheck.Instances.OldTime\n    Test.QuickCheck.Instances.Primitive\n    Test.QuickCheck.Instances.Scientific\n    Test.QuickCheck.Instances.Semigroup\n    Test.QuickCheck.Instances.Solo\n    Test.QuickCheck.Instances.Strict\n    Test.QuickCheck.Instances.Tagged\n    Test.QuickCheck.Instances.Text\n    Test.QuickCheck.Instances.These\n    Test.QuickCheck.Instances.Time\n    Test.QuickCheck.Instances.Transformer\n    Test.QuickCheck.Instances.UnorderedContainers\n    Test.QuickCheck.Instances.UUID\n    Test.QuickCheck.Instances.Vector\n    Test.QuickCheck.Instances.Void\n\n  other-modules:    Test.QuickCheck.Instances.CustomPrelude\n  hs-source-dirs:   src\n  build-depends:\n      base        >=4.5    && <4.18\n    , QuickCheck  >=2.14.1 && <2.14.3\n    , splitmix    >=0.0.2  && <0.2\n\n  build-depends:\n      array                 >=0.4.0.0  && <0.6\n    , bytestring            >=0.9.2.1  && <0.12\n    , case-insensitive      >=1.2.0.4  && <1.3\n    , containers            >=0.4.2.1  && <0.7\n    , data-fix              >=0.3      && <0.4\n    , hashable              >=1.2.7.0  && <1.5\n    , integer-logarithms    >=1.0.3    && <1.1\n    , old-time              >=1.1.0.0  && <1.2\n    , OneTuple              >=0.3      && <0.4\n    , primitive             >=0.6.4.0  && <0.8\n    , scientific            >=0.3.6.2  && <0.4\n    , strict                >=0.4      && <0.5\n    , tagged                >=0.8.6    && <0.9\n    , text                  >=1.2.3.0  && <1.3 || >=2.0 && <2.1\n    , these                 >=1.1.1.1  && <1.2\n    , time-compat           >=1.9.4    && <1.10\n    , transformers          >=0.3.0.0  && <0.7\n    , transformers-compat   >=0.6.5    && <0.8\n    , unordered-containers  >=0.2.2.0  && <0.3\n    , uuid-types            >=1.0.3    && <1.1\n    , vector                >=0.12.3.1 && <0.14\n\n  -- version is irrelevant.\n  build-depends:    time\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.5 && <0.21\n\n  if !impl(ghc >=7.10)\n    build-depends:\n        nats  >=1.1.2 && <1.2\n      , void  >=0.7.2 && <0.8\n\n  if flag(bytestring-builder)\n    build-depends:\n        bytestring          <0.10.4.0\n      , bytestring-builder  >=0.10.4 && <0.11\n\n  else\n    build-depends: bytestring >=0.10.4.0\n\n  if impl(ghc >=7.8)\n    exposed-modules: Test.QuickCheck.Instances.Text.Short\n    build-depends:   text-short >=0.1.3 && <0.2\n\n  ghc-options:      -Wall\n\ntest-suite self-test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   test\n  build-depends:\n      base\n    , containers\n    , primitive\n    , QuickCheck\n    , quickcheck-instances\n    , tagged\n    , uuid-types\n\nbenchmark bytestring-gen\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          ByteString.hs\n  hs-source-dirs:   bench\n  build-depends:\n      base\n    , bytestring\n    , QuickCheck\n    , quickcheck-instances\n";
    }