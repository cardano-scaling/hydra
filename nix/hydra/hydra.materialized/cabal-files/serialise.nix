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
    flags = { newtime15 = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "serialise"; version = "0.2.6.0"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Duncan Coutts,\n2015-2017 Well-Typed LLP,\n2015 IRIS Connect Ltd";
      maintainer = "duncan@community.haskell.org, ben@smart-cactus.org";
      author = "Duncan Coutts";
      homepage = "https://github.com/well-typed/cborg";
      url = "";
      synopsis = "A binary serialisation library for Haskell values.";
      description = "This package (formerly @binary-serialise-cbor@) provides pure, efficient\nserialization of Haskell values directly into @ByteString@s for storage or\ntransmission purposes. By providing a set of type class instances, you can\nalso serialise any custom data type you have as well.\n\nThe underlying binary format used is the 'Concise Binary Object\nRepresentation', or CBOR, specified in RFC 7049. As a result,\nserialised Haskell values have implicit structure outside of the\nHaskell program itself, meaning they can be inspected or analyzed\nwithout custom tools.\n\nAn implementation of the standard bijection between CBOR and JSON is provided\nby the [cborg-json](/package/cborg-json) package. Also see\n[cbor-tool](/package/cbor-tool) for a convenient command-line utility for\nworking with CBOR data.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."half" or (errorHandler.buildDepError "half"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (if flags.newtime15
          then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
          else [
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            ]);
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "instances" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ] ++ (if flags.newtime15
            then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
              ]);
          buildable = true;
          };
        "micro" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."cereal-vector" or (errorHandler.buildDepError "cereal-vector"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."store" or (errorHandler.buildDepError "store"))
            ];
          buildable = true;
          };
        "versus" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."half" or (errorHandler.buildDepError "half"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."store" or (errorHandler.buildDepError "store"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ] ++ (if flags.newtime15
            then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
              ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/serialise-0.2.6.0.tar.gz";
      sha256 = "93ff1888e1972999f14663072b38efcfd0c1481b4ec8e30ddc9c5ce97681a516";
      });
    }) // {
    package-description-override = "name:                serialise\nversion:             0.2.6.0\nsynopsis:            A binary serialisation library for Haskell values.\ndescription:\n  This package (formerly @binary-serialise-cbor@) provides pure, efficient\n  serialization of Haskell values directly into @ByteString@s for storage or\n  transmission purposes. By providing a set of type class instances, you can\n  also serialise any custom data type you have as well.\n  .\n  The underlying binary format used is the 'Concise Binary Object\n  Representation', or CBOR, specified in RFC 7049. As a result,\n  serialised Haskell values have implicit structure outside of the\n  Haskell program itself, meaning they can be inspected or analyzed\n  without custom tools.\n  .\n  An implementation of the standard bijection between CBOR and JSON is provided\n  by the [cborg-json](/package/cborg-json) package. Also see\n  [cbor-tool](/package/cbor-tool) for a convenient command-line utility for\n  working with CBOR data.\nhomepage:            https://github.com/well-typed/cborg\nlicense:             BSD3\nlicense-file:        LICENSE.txt\nauthor:              Duncan Coutts\nmaintainer:          duncan@community.haskell.org, ben@smart-cactus.org\nbug-reports:         https://github.com/well-typed/cborg/issues\ncopyright:           2015-2017 Duncan Coutts,\n                     2015-2017 Well-Typed LLP,\n                     2015 IRIS Connect Ltd\ncabal-version:       >=1.10\ncategory:            Codec\nbuild-type:          Simple\ntested-with:\n  GHC == 8.4.4,\n  GHC == 8.6.5,\n  GHC == 8.8.3,\n  GHC == 8.10.7,\n  GHC == 9.0.1,\n  GHC == 9.2.2,\n  GHC == 9.4.2\n\nextra-source-files:\n  ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/well-typed/cborg.git\n\n--------------------------------------------------------------------------------\n-- Flags\n\nflag newtime15\n  default: True\n  manual: False\n  description: Use the new time 1.5 library\n\nlibrary\n  default-language:  Haskell2010\n  ghc-options:       -Wall\n  hs-source-dirs:    src\n\n  exposed-modules:\n    Codec.Serialise\n    Codec.Serialise.Class\n    Codec.Serialise.Decoding\n    Codec.Serialise.Encoding\n    Codec.Serialise.IO\n    Codec.Serialise.Properties\n    Codec.Serialise.Tutorial\n    Codec.Serialise.Internal.GeneralisedUTF8\n\n  build-depends:\n    base                    >= 4.11    && < 4.18,\n    array                   >= 0.4     && < 0.6,\n    bytestring              >= 0.10.4  && < 0.12,\n    cborg                   == 0.2.*,\n    containers              >= 0.5     && < 0.7,\n    ghc-prim                >= 0.3.1.0 && < 0.10,\n    half                    >= 0.2.2.3 && < 0.4,\n    hashable                >= 1.2     && < 2.0,\n    primitive               >= 0.5     && < 0.8,\n    strict                  >= 0.4     && < 0.5,\n    text                    >= 1.1     && < 2.1,\n    these                   >= 1.1     && < 1.2,\n    unordered-containers    >= 0.2     && < 0.3,\n    vector                  >= 0.10    && < 0.13\n\n  if flag(newtime15)\n    build-depends:\n      time                  >= 1.5     && < 1.14\n  else\n    build-depends:\n      time                  >= 1.4     && < 1.5,\n      old-locale\n\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n\n--------------------------------------------------------------------------------\n-- Tests\n\ntest-suite tests\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    tests\n  main-is:           Main.hs\n\n  default-language:  Haskell2010\n  ghc-options:\n    -Wall -fno-warn-orphans\n    -threaded -rtsopts \"-with-rtsopts=-N2\"\n\n  other-modules:\n    Tests.IO\n    Tests.Negative\n    Tests.Orphanage\n    Tests.Serialise\n    Tests.Serialise.Canonical\n    Tests.Regress\n    Tests.Regress.Issue13\n    Tests.Regress.Issue67\n    Tests.Regress.Issue80\n    Tests.Regress.Issue106\n    Tests.Regress.Issue135\n    Tests.Deriving\n    Tests.GeneralisedUTF8\n\n  build-depends:\n    base                    >= 4.11    && < 4.18,\n    bytestring              >= 0.10.4  && < 0.12,\n    directory               >= 1.0     && < 1.4,\n    filepath                >= 1.0     && < 1.5,\n    text                    >= 1.1     && < 2.1,\n    time                    >= 1.4     && < 1.14,\n    containers              >= 0.5     && < 0.7,\n    unordered-containers    >= 0.2     && < 0.3,\n    primitive               >= 0.5     && < 0.8,\n    cborg,\n    serialise,\n    QuickCheck              >= 2.9     && < 2.15,\n    tasty                   >= 0.11    && < 1.5,\n    tasty-hunit             >= 0.9     && < 0.11,\n    tasty-quickcheck        >= 0.8     && < 0.11,\n    quickcheck-instances    >= 0.3.12  && < 0.4,\n    vector                  >= 0.10    && < 0.13\n\n--------------------------------------------------------------------------------\n-- Benchmarks\n\nbenchmark instances\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    bench/instances\n  main-is:           Main.hs\n\n  default-language:  Haskell2010\n  ghc-options:\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans\n\n  other-modules:\n    Instances.Float\n    Instances.Integer\n    Instances.Vector\n    Instances.Time\n\n  build-depends:\n    base                    >= 4.11    && < 4.18,\n    binary                  >= 0.7     && < 0.11,\n    bytestring              >= 0.10.4  && < 0.12,\n    vector                  >= 0.10    && < 0.13,\n    cborg,\n    serialise,\n\n    deepseq                 >= 1.0     && < 1.5,\n    criterion               >= 1.0     && < 1.6\n\n  if flag(newtime15)\n    build-depends:\n      time                  >= 1.5     && < 1.14\n  else\n    build-depends:\n      time                  >= 1.4     && < 1.5,\n      old-locale\n\nbenchmark micro\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    bench/micro\n  main-is:           Main.hs\n\n  default-language:  Haskell2010\n  ghc-options:\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans\n\n  other-modules:\n    Micro\n    Micro.Types\n    Micro.Load\n    Micro.DeepSeq\n    Micro.MemSize\n    Micro.ReadShow\n    Micro.PkgAesonGeneric\n    Micro.PkgAesonTH\n    Micro.PkgBinary\n    Micro.PkgCereal\n    Micro.PkgStore\n    Micro.CBOR\n\n    SimpleVersus\n\n  build-depends:\n    base                    >= 4.11    && < 4.18,\n    binary                  >= 0.7     && < 0.11,\n    bytestring              >= 0.10.4  && < 0.12,\n    ghc-prim                >= 0.3.1.0 && < 0.10,\n    vector                  >= 0.10    && < 0.13,\n    cborg,\n    serialise,\n\n    aeson                   >= 0.7     && < 2.2,\n    deepseq                 >= 1.0     && < 1.5,\n    criterion               >= 1.0     && < 1.6,\n    cereal                  >= 0.5.2.0 && < 0.6,\n    cereal-vector           >= 0.2     && < 0.3,\n    semigroups              >= 0.18    && < 0.21,\n    store                   >= 0.7.1   && < 0.8\n\nbenchmark versus\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    bench/versus\n  main-is:           Main.hs\n\n  default-language:  Haskell2010\n  ghc-options:\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans\n\n  other-modules:\n    Utils\n\n    -- Suite #1\n    Mini\n\n    -- Suite #2\n    Macro\n    Macro.Types\n    Macro.Load\n    Macro.DeepSeq\n    Macro.MemSize\n    Macro.ReadShow\n    Macro.PkgAesonGeneric\n    Macro.PkgAesonTH\n    Macro.PkgBinary\n    Macro.PkgCereal\n    Macro.PkgStore\n    Macro.CBOR\n\n  build-depends:\n    base                    >= 4.11    && < 4.18,\n    array                   >= 0.4     && < 0.6,\n    binary                  >= 0.7     && < 0.11,\n    bytestring              >= 0.10.4  && < 0.12,\n    directory               >= 1.0     && < 1.4,\n    ghc-prim                >= 0.3.1.0 && < 0.10,\n    fail                    >= 4.9.0.0 && < 4.10,\n    text                    >= 1.1     && < 2.1,\n    vector                  >= 0.10    && < 0.13,\n    cborg,\n    serialise,\n\n    filepath                >= 1.0     && < 1.5,\n    containers              >= 0.5     && < 0.7,\n    deepseq                 >= 1.0     && < 1.5,\n    aeson                   >= 0.7     && < 2.2,\n    cereal                  >= 0.5.2.0 && < 0.6,\n    half                    >= 0.2.2.3 && < 0.4,\n    tar                     >= 0.4     && < 0.6,\n    zlib                    >= 0.5     && < 0.7,\n    pretty                  >= 1.0     && < 1.2,\n    criterion               >= 1.0     && < 1.6,\n    store                   >= 0.7.1   && < 0.8,\n    semigroups\n\n  if flag(newtime15)\n    build-depends:\n      time                  >= 1.5     && < 1.14\n  else\n    build-depends:\n      time                  >= 1.4     && < 1.5,\n      old-locale\n";
    }