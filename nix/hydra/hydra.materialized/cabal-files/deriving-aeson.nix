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
      specVersion = "2.4";
      identifier = { name = "deriving-aeson"; version = "0.2.8"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2020 Fumiaki Kinoshita";
      maintainer = "fumiexcel@gmail.com";
      author = "Fumiaki Kinoshita";
      homepage = "";
      url = "";
      synopsis = "Type driven generic aeson instance customisation";
      description = "This package provides a newtype wrapper with\nFromJSON/ToJSON instances customisable via a phantom type parameter.\nThe instances can be rendered to the original type using DerivingVia.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/deriving-aeson-0.2.8.tar.gz";
      sha256 = "2dd3824fbb182811655e7ff9a2292ffc9178a29d0ccdcc0d04ef74c54856a938";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\r\nname:                deriving-aeson\r\nversion:             0.2.8\r\nx-revision: 1\r\nsynopsis:            Type driven generic aeson instance customisation\r\ndescription:         This package provides a newtype wrapper with\r\n  FromJSON/ToJSON instances customisable via a phantom type parameter.\r\n  The instances can be rendered to the original type using DerivingVia.\r\nbug-reports:         https://github.com/fumieval/deriving-aeson\r\nlicense:             BSD-3-Clause\r\nlicense-file:        LICENSE\r\nauthor:              Fumiaki Kinoshita\r\nmaintainer:          fumiexcel@gmail.com\r\ncopyright:           Copyright (c) 2020 Fumiaki Kinoshita\r\ncategory:            JSON, Generics\r\nextra-source-files:  CHANGELOG.md, README.md\r\ntested-with:         GHC == 8.6.5, GHC == 8.8.3, GHC == 8.10.4, GHC == 9.2.1\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/fumieval/deriving-aeson.git\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Deriving.Aeson\r\n    Deriving.Aeson.Stock\r\n  build-depends:       base >= 4.12 && <5, aeson >= 1.4.7.0 && <2.2\r\n  hs-source-dirs: src\r\n  default-language:    Haskell2010\r\n  ghc-options: -Wall -Wcompat\r\n\r\ntest-suite test\r\n  type: exitcode-stdio-1.0\r\n  main-is: test.hs\r\n  build-depends: base, aeson, deriving-aeson, bytestring\r\n  hs-source-dirs: tests\r\n  default-language:    Haskell2010\r\n";
    }