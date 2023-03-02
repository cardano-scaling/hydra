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
      identifier = { name = "hedgehog"; version = "1.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Jacob Stanley";
      homepage = "https://hedgehog.qa";
      url = "";
      synopsis = "Release with confidence.";
      description = "<http://hedgehog.qa/ Hedgehog> automatically generates a comprehensive array\nof test cases, exercising your software in ways human testers would never\nimagine.\n\nGenerate hundreds of test cases automatically, exposing even the\nmost insidious of corner cases. Failures are automatically simplified, giving\ndevelopers coherent, intelligible error messages.\n\nTo get started quickly, see the <https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example examples>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."concurrent-output" or (errorHandler.buildDepError "concurrent-output"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."erf" or (errorHandler.buildDepError "erf"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."wl-pprint-annotated" or (errorHandler.buildDepError "wl-pprint-annotated"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hedgehog-1.1.2.tar.gz";
      sha256 = "e1c95456b3ddf815c4b863c8f8229e82548887d1835fdae4fc17433361397335";
      });
    }) // {
    package-description-override = "version: 1.1.2\n\nname:\n  hedgehog\nauthor:\n  Jacob Stanley\nmaintainer:\n  Jacob Stanley <jacob@stanley.io>\nhomepage:\n  https://hedgehog.qa\nbug-reports:\n  https://github.com/hedgehogqa/haskell-hedgehog/issues\nsynopsis:\n  Release with confidence.\ndescription:\n  <http://hedgehog.qa/ Hedgehog> automatically generates a comprehensive array\n  of test cases, exercising your software in ways human testers would never\n  imagine.\n  .\n  Generate hundreds of test cases automatically, exposing even the\n  most insidious of corner cases. Failures are automatically simplified, giving\n  developers coherent, intelligible error messages.\n  .\n  To get started quickly, see the <https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example examples>.\ncategory:\n  Testing\nlicense:\n  BSD3\nlicense-file:\n  LICENSE\ncabal-version:\n  >= 1.10\nbuild-type:\n  Simple\ntested-with:\n    GHC == 8.0.2\n  , GHC == 8.2.2\n  , GHC == 8.4.4\n  , GHC == 8.6.5\n  , GHC == 8.8.3\n  , GHC == 8.10.1\n  , GHC == 9.2.1\nextra-source-files:\n  README.md\n  CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: git://github.com/hedgehogqa/haskell-hedgehog.git\n\nlibrary\n  build-depends:\n   -- GHC 8.0.1 / base-4.9.0.0 (May 2016)\n      base                            >= 4.9        && < 5\n    , ansi-terminal                   >= 0.6        && < 0.12\n    , async                           >= 2.0        && < 2.3\n    , barbies                         >= 1.0        && < 2.1\n    , bytestring                      >= 0.10       && < 0.12\n    , concurrent-output               >= 1.7        && < 1.11\n    , containers                      >= 0.4        && < 0.7\n    , deepseq                         >= 1.1.0.0    && < 1.5\n    , directory                       >= 1.2        && < 1.4\n    , erf                             >= 2.0        && < 2.1\n    , exceptions                      >= 0.7        && < 0.11\n    , lifted-async                    >= 0.7        && < 0.11\n    , mmorph                          >= 1.0        && < 1.3\n    , monad-control                   >= 1.0        && < 1.1\n    , mtl                             >= 2.1        && < 2.3\n    , pretty-show                     >= 1.6        && < 1.11\n    , primitive                       >= 0.6        && < 0.8\n    , random                          >= 1.1        && < 1.3\n    , resourcet                       >= 1.1        && < 1.3\n    , stm                             >= 2.4        && < 2.6\n    , template-haskell                >= 2.10       && < 2.20\n    , text                            >= 1.1        && < 2.1\n    , time                            >= 1.4        && < 1.13\n    , transformers                    >= 0.5        && < 0.6\n    , transformers-base               >= 0.4.5.1    && < 0.5\n    , wl-pprint-annotated             >= 0.0        && < 0.2\n\n  ghc-options:\n    -Wall\n\n  hs-source-dirs:\n    src\n\n  exposed-modules:\n    Hedgehog\n    Hedgehog.Gen\n    Hedgehog.Main\n    Hedgehog.Range\n\n    Hedgehog.Internal.Barbie\n    Hedgehog.Internal.Config\n    Hedgehog.Internal.Discovery\n    Hedgehog.Internal.Distributive\n    Hedgehog.Internal.Exception\n    Hedgehog.Internal.Gen\n    Hedgehog.Internal.HTraversable\n    Hedgehog.Internal.Opaque\n    Hedgehog.Internal.Prelude\n    Hedgehog.Internal.Property\n    Hedgehog.Internal.Queue\n    Hedgehog.Internal.Range\n    Hedgehog.Internal.Region\n    Hedgehog.Internal.Report\n    Hedgehog.Internal.Runner\n    Hedgehog.Internal.Seed\n    Hedgehog.Internal.Show\n    Hedgehog.Internal.Shrink\n    Hedgehog.Internal.Source\n    Hedgehog.Internal.State\n    Hedgehog.Internal.TH\n    Hedgehog.Internal.Tree\n    Hedgehog.Internal.Tripping\n\n  default-language:\n    Haskell2010\n\ntest-suite test\n  type:\n    exitcode-stdio-1.0\n\n  main-is:\n    test.hs\n\n  ghc-options:\n    -Wall -threaded -O2\n\n  hs-source-dirs:\n    test\n\n  other-modules:\n    Test.Hedgehog.Applicative\n    Test.Hedgehog.Confidence\n    Test.Hedgehog.Filter\n    Test.Hedgehog.Maybe\n    Test.Hedgehog.Seed\n    Test.Hedgehog.Text\n    Test.Hedgehog.Zip\n\n  build-depends:\n      hedgehog\n    , base                            >= 3          && < 5\n    , containers                      >= 0.4        && < 0.7\n    , mmorph                          >= 1.0        && < 1.3\n    , mtl                             >= 2.1        && < 2.3\n    , pretty-show                     >= 1.6        && < 1.11\n    , text                            >= 1.1        && < 1.3\n    , transformers                    >= 0.3        && < 0.6\n\n  default-language:\n    Haskell2010\n";
    }