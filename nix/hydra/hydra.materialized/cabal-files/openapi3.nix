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
      specVersion = "1.10";
      identifier = { name = "openapi3"; version = "3.2.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2015-2016, GetShopTV, (c) 2020, Biocad";
      maintainer = "nickolay@getshoptv.com, kolmax94@gmail.com";
      author = "Nickolay Kudasov, Maxim Koltsov";
      homepage = "https://github.com/biocad/openapi3";
      url = "";
      synopsis = "OpenAPI 3.0 data model";
      description = "This library is intended to be used for decoding and encoding OpenAPI 3.0 API\nspecifications as well as manipulating them.\n\nThe original OpenAPI 3.0 specification is available at http://swagger.io/specification/.";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."insert-ordered-containers" or (errorHandler.buildDepError "insert-ordered-containers"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."optics-core" or (errorHandler.buildDepError "optics-core"))
          (hsPkgs."optics-th" or (errorHandler.buildDepError "optics-th"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      exes = {
        "example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."insert-ordered-containers" or (errorHandler.buildDepError "insert-ordered-containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/openapi3-3.2.2.tar.gz";
      sha256 = "325d491e305de60510f5267b3eccdc2719d4a8e5784577d7e79aef28368d6134";
      });
    }) // {
    package-description-override = "cabal-version:       >=1.10\r\nname:                openapi3\r\nversion:             3.2.2\r\nx-revision: 1\r\n\r\nsynopsis:            OpenAPI 3.0 data model\r\ncategory:            Web, Swagger, OpenApi\r\ndescription:\r\n  This library is intended to be used for decoding and encoding OpenAPI 3.0 API\r\n  specifications as well as manipulating them.\r\n  .\r\n  The original OpenAPI 3.0 specification is available at http://swagger.io/specification/.\r\n\r\nhomepage:            https://github.com/biocad/openapi3\r\nbug-reports:         https://github.com/biocad/openapi3/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              Nickolay Kudasov, Maxim Koltsov\r\nmaintainer:          nickolay@getshoptv.com, kolmax94@gmail.com\r\ncopyright:           (c) 2015-2016, GetShopTV, (c) 2020, Biocad\r\nbuild-type:          Custom\r\nextra-source-files:\r\n    README.md\r\n  , CHANGELOG.md\r\n  , examples/*.hs\r\ntested-with:\r\n  GHC ==8.4.4\r\n   || ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.2\r\n\r\ncustom-setup\r\n  setup-depends:\r\n    base, Cabal, cabal-doctest >=1.0.6 && <1.1\r\n\r\nlibrary\r\n  hs-source-dirs:      src\r\n  exposed-modules:\r\n    Data.OpenApi\r\n    Data.OpenApi.Declare\r\n    Data.OpenApi.Lens\r\n    Data.OpenApi.Operation\r\n    Data.OpenApi.Optics\r\n    Data.OpenApi.ParamSchema\r\n    Data.OpenApi.Schema\r\n    Data.OpenApi.Schema.Generator\r\n    Data.OpenApi.Schema.Validation\r\n    Data.OpenApi.SchemaOptions\r\n\r\n    -- internal modules\r\n    Data.OpenApi.Internal\r\n    Data.OpenApi.Internal.Schema\r\n    Data.OpenApi.Internal.Schema.Validation\r\n    Data.OpenApi.Internal.ParamSchema\r\n    Data.OpenApi.Internal.Utils\r\n    Data.OpenApi.Internal.AesonUtils\r\n    Data.OpenApi.Internal.TypeShape\r\n\r\n    Data.OpenApi.Aeson.Compat\r\n\r\n  -- GHC boot libraries\r\n  build-depends:\r\n      base             >=4.11.1.0  && <4.17\r\n    , bytestring       >=0.10.8.2  && <0.12\r\n    , containers       >=0.5.11.0  && <0.7\r\n    , template-haskell >=2.13.0.0  && <2.19\r\n    , time             >=1.8.0.2   && <1.14\r\n    , transformers     >=0.5.5.0   && <0.6\r\n\r\n  build-depends:\r\n      mtl              >=2.2.2   && <2.3\r\n    , text             >=1.2.3.1 && <2.1\r\n\r\n  -- other dependencies\r\n  build-depends:\r\n      base-compat-batteries     >=0.11.1   && <0.13\r\n    , aeson                     >=1.4.2.0  && <1.6 || >=2.0.1.0 && < 2.1\r\n    , aeson-pretty              >=0.8.7    && <0.9\r\n    -- cookie 0.4.3 is needed by GHC 7.8 due to time>=1.4 constraint\r\n    , cookie                    >=0.4.3    && <0.5\r\n    , generics-sop              >=0.5.1.0  && <0.6\r\n    , hashable                  >=1.2.7.0  && <1.5\r\n    , http-media                >=0.8.0.0  && <0.9\r\n    , insert-ordered-containers >=0.2.3    && <0.3\r\n    , lens                      >=4.16.1   && <5.2\r\n    , network                   >=2.6.3.5  && <3.2\r\n    , optics-core               >=0.2      && <0.5\r\n    , optics-th                 >=0.2      && <0.5\r\n    , scientific                >=0.3.6.2  && <0.4\r\n    , unordered-containers      >=0.2.9.0  && <0.3\r\n    , uuid-types                >=1.0.3    && <1.1\r\n    , vector                    >=0.12.0.1 && <0.13\r\n    , QuickCheck                >=2.10.1   && <2.15\r\n\r\n  default-language:    Haskell2010\r\n\r\ntest-suite spec\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   test\r\n  main-is:          Spec.hs\r\n\r\n  -- From library parat\r\n -- We need aeson's toEncoding for doctests too\r\n  build-depends:\r\n      base\r\n    , QuickCheck\r\n    , aeson\r\n    , base-compat-batteries\r\n    , bytestring\r\n    , containers\r\n    , hashable\r\n    , insert-ordered-containers\r\n    , lens\r\n    , mtl\r\n    , openapi3\r\n    , template-haskell\r\n    , text\r\n    , time\r\n    , unordered-containers\r\n    , vector\r\n\r\n  -- test-suite only dependencies\r\n  build-depends:\r\n      hspec                >=2.5.5   && <2.11\r\n    , HUnit                >=1.6.0.0 && <1.7\r\n    , quickcheck-instances >=0.3.19  && <0.14\r\n    , utf8-string          >=1.0.1.1 && <1.1\r\n\r\n  -- https://github.com/haskell/cabal/issues/3708\r\n  build-tool-depends:\r\n    hspec-discover:hspec-discover >=2.5.5 && <2.11\r\n\r\n  other-modules:\r\n    SpecCommon\r\n    Data.OpenApiSpec\r\n    Data.OpenApi.CommonTestTypes\r\n    Data.OpenApi.ParamSchemaSpec\r\n    Data.OpenApi.SchemaSpec\r\n    Data.OpenApi.Schema.ValidationSpec\r\n    Data.OpenApi.Schema.GeneratorSpec\r\n  default-language: Haskell2010\r\n\r\ntest-suite doctests\r\n  -- See QuickCheck note in https://github.com/phadej/cabal-doctest#notes\r\n  build-depends:    base, doctest, Glob, QuickCheck\r\n  default-language: Haskell2010\r\n  hs-source-dirs:   test\r\n  main-is:          doctests.hs\r\n  type:             exitcode-stdio-1.0\r\n\r\nexecutable example\r\n  hs-source-dirs:   examples\r\n  main-is:          hackage.hs\r\n  default-language: Haskell2010\r\n  build-depends:    base, aeson, lens, openapi3, text\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/biocad/openapi3.git\r\n";
    }