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
    flags = { buildexe = true; buildexample = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "pretty-simple"; version = "4.1.2.0"; };
      license = "BSD-3-Clause";
      copyright = "2017-2019 Dennis Gosnell";
      maintainer = "cdep.illabout@gmail.com";
      author = "Dennis Gosnell";
      homepage = "https://github.com/cdepillabout/pretty-simple";
      url = "";
      synopsis = "pretty printer for data types with a 'Show' instance.";
      description = "Please see <https://github.com/cdepillabout/pretty-simple#readme README.md>.";
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
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      exes = {
        "pretty-simple" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = if flags.buildexe then true else false;
          };
        "pretty-simple-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            ];
          buildable = if flags.buildexample then true else false;
          };
        "pretty-simple-json-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if flags.buildexample then true else false;
          };
        };
      tests = {
        "pretty-simple-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "pretty-simple-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/pretty-simple-4.1.2.0.tar.gz";
      sha256 = "3cb47c3971599cb8c0330e1a2dbd63c67bfbe570bd879524ee802e81e7b02736";
      });
    }) // {
    package-description-override = "name:                pretty-simple\nversion:             4.1.2.0\nsynopsis:            pretty printer for data types with a 'Show' instance.\ndescription:         Please see <https://github.com/cdepillabout/pretty-simple#readme README.md>.\nhomepage:            https://github.com/cdepillabout/pretty-simple\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Dennis Gosnell\nmaintainer:          cdep.illabout@gmail.com\ncopyright:           2017-2019 Dennis Gosnell\ncategory:            Text\nbuild-type:          Custom\nextra-source-files:  CHANGELOG.md\n                   , README.md\n                   , img/pretty-simple-example-screenshot.png\ncabal-version:       >=1.10\n\ncustom-setup\n  setup-depends:     base\n                   , Cabal >= 1.24\n                   , cabal-doctest >=1.0.2\n\nflag buildexe\n  description: Build an small command line program that pretty-print anything from stdin.\n  default:     True\n\nflag buildexample\n  description: Build a small example program showing how to use the pPrint function\n  default:     False\n\nlibrary\n  hs-source-dirs:      src\n  exposed-modules:     Debug.Pretty.Simple\n                     , Text.Pretty.Simple\n                     , Text.Pretty.Simple.Internal\n                     , Text.Pretty.Simple.Internal.Color\n                     , Text.Pretty.Simple.Internal.Expr\n                     , Text.Pretty.Simple.Internal.ExprParser\n                     , Text.Pretty.Simple.Internal.Printer\n  build-depends:       base >= 4.8 && < 5\n                     , containers\n                     , mtl >= 2.2\n                     , prettyprinter >= 1.7.0\n                     , prettyprinter-ansi-terminal >= 1.1.2\n                     , text >= 1.2\n                     , transformers >= 0.4\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n  other-extensions:    TemplateHaskell\n\nexecutable pretty-simple\n  main-is:             Main.hs\n  other-modules:       Paths_pretty_simple\n  hs-source-dirs:      app\n  build-depends:       base\n                     , pretty-simple\n                     , text\n                     , optparse-applicative\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n\n  if flag(buildexe)\n    buildable:       True\n  else\n    buildable:       False\n\n\nexecutable pretty-simple-example\n  main-is:             Example.hs\n  other-modules:       Example.Data\n  hs-source-dirs:      example\n  build-depends:       base\n                     , pretty-simple\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n\n  if flag(buildexample)\n    buildable:       True\n  else\n    buildable:       False\n\nexecutable pretty-simple-json-example\n  main-is:             ExampleJSON.hs\n  other-modules:       Example.Data\n  hs-source-dirs:      example\n  build-depends:       base\n                     , aeson\n                     , bytestring\n                     , pretty-simple\n                     , text\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n\n  if flag(buildexample)\n    buildable:       True\n  else\n    buildable:       False\n\ntest-suite pretty-simple-doctest\n  type:                exitcode-stdio-1.0\n  main-is:             DocTest.hs\n  hs-source-dirs:      test\n  build-depends:       base\n                     , doctest >= 0.13\n                     , Glob\n                     , QuickCheck\n                     , template-haskell\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n\nbenchmark pretty-simple-bench\n  type:                exitcode-stdio-1.0\n  main-is:             Bench.hs\n  other-modules:       Example.Data\n  hs-source-dirs:      bench\n                     , example\n  build-depends:       base\n                     , criterion\n                     , pretty-simple\n                     , text\n  default-language:    Haskell2010\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N\n\nsource-repository head\n  type:     git\n  location: git@github.com:cdepillabout/pretty-simple.git\n";
    }