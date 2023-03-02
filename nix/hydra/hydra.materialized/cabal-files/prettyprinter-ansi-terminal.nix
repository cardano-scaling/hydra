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
      identifier = { name = "prettyprinter-ansi-terminal"; version = "1.1.3"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Simon Jakobi <simon.jakobi@gmail.com>, David Luposchainsky <dluposchainsky at google>";
      author = "David Luposchainsky";
      homepage = "http://github.com/quchen/prettyprinter";
      url = "";
      synopsis = "ANSI terminal backend for the »prettyprinter« package.";
      description = "See README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "7.10"
            then false
            else true;
          };
        };
      benchmarks = {
        "large-output" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-ansi-terminal" or (errorHandler.buildDepError "prettyprinter-ansi-terminal"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/prettyprinter-ansi-terminal-1.1.3.tar.gz";
      sha256 = "813739308ad6050620578994effe21058a170a341716acf52573fae42b5b1db3";
      });
    }) // {
    package-description-override = "name:                prettyprinter-ansi-terminal\nversion:             1.1.3\ncabal-version:       >= 1.10\ncategory:            User Interfaces, Text\nsynopsis:            ANSI terminal backend for the »prettyprinter« package.\ndescription:         See README.md\nlicense:             BSD2\nlicense-file:        LICENSE.md\nextra-source-files:  README.md\n                   , misc/version-compatibility-macros.h\n                   , CHANGELOG.md\nauthor:              David Luposchainsky\nmaintainer:          Simon Jakobi <simon.jakobi@gmail.com>, David Luposchainsky <dluposchainsky at google>\nbug-reports:         http://github.com/quchen/prettyprinter/issues\nhomepage:            http://github.com/quchen/prettyprinter\nbuild-type:          Simple\ntested-with:         GHC==9.0.1, GHC==8.10.4, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2\n\nsource-repository head\n  type: git\n  location: git://github.com/quchen/prettyprinter.git\n\nlibrary\n    exposed-modules:  Data.Text.Prettyprint.Doc.Render.Terminal\n                    , Data.Text.Prettyprint.Doc.Render.Terminal.Internal\n                    , Prettyprinter.Render.Terminal\n                    , Prettyprinter.Render.Terminal.Internal\n    ghc-options:      -Wall -O2\n    hs-source-dirs:   src\n    include-dirs:     misc\n    default-language: Haskell2010\n    other-extensions:\n          CPP\n        , OverloadedStrings\n\n\n    build-depends:\n          base          >= 4.5 && < 5\n        , ansi-terminal >= 0.4.0\n        , text          >= 1.2\n        , prettyprinter >= 1.7.0\n\n    if impl(ghc >= 8.0)\n        ghc-options: -Wcompat\n    if !impl(ghc >= 8.0)\n        build-depends: semigroups >= 0.1\n\ntest-suite doctest\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test/Doctest\n    main-is: Main.hs\n    build-depends:\n          base    >= 4.7 && < 5\n        , doctest >= 0.9\n    ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\n    default-language: Haskell2010\n    if impl (ghc < 7.10)\n        buildable: False\n        -- Doctest cannot search folders in old versions it seems :-(\n\nbenchmark large-output\n    build-depends:\n          base >= 4.5 && < 5\n        , base-compat >=0.9.3 && <0.12\n        , containers\n        , deepseq\n        , gauge >= 0.2\n        , prettyprinter\n        , prettyprinter-ansi-terminal\n        , QuickCheck >= 2.7\n        , text\n\n    hs-source-dirs:      bench\n    main-is:             LargeOutput.hs\n    ghc-options:         -O2 -rtsopts -Wall\n    default-language:    Haskell2010\n    type:                exitcode-stdio-1.0\n";
    }