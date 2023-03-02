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
      identifier = { name = "microstache"; version = "1.0.2.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus<oleg.grenrus@iki.fi>";
      author = "Mark Karpov <markkarpov@openmailbox.org>, Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/haskellari/microstache";
      url = "";
      synopsis = "Mustache templates for Haskell";
      description = "Mustache templates for Haskell.\n\nBased on @stache@ library, which uses @megaparsec@.\nThis library uses @parsec@, thus the name: @microstache@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).le "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        "mustache-spec" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/microstache-1.0.2.2.tar.gz";
      sha256 = "f0a1dbef45a137e1af0e29ea4b9585788217cc1c6f1db7d68a4f659916dd36ac";
      });
    }) // {
    package-description-override = "name:               microstache\nversion:            1.0.2.2\ncabal-version:      >=1.10\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:\n  Mark Karpov <markkarpov@openmailbox.org>, Oleg Grenrus <oleg.grenrus@iki.fi>\n\nmaintainer:         Oleg Grenrus<oleg.grenrus@iki.fi>\nhomepage:           https://github.com/haskellari/microstache\nbug-reports:        https://github.com/haskellari/microstache/issues\ncategory:           Text\nsynopsis:           Mustache templates for Haskell\nbuild-type:         Simple\ndescription:\n  Mustache templates for Haskell.\n  .\n  Based on @stache@ library, which uses @megaparsec@.\n  This library uses @parsec@, thus the name: @microstache@.\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n  specification/comments.json\n  specification/delimiters.json\n  specification/interpolation.json\n  specification/inverted.json\n  specification/partials.json\n  specification/sections.json\n\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/microstache.git\n\nlibrary\n  build-depends:\n      aeson                 >=0.11    && <1.6 || >=2.0.0.0 && <2.2\n    , base                  >=4.5     && <4.18\n    , containers            >=0.4.2.1 && <0.7\n    , deepseq               >=1.3.0.0 && <1.5\n    , directory             >=1.1.0.2 && <1.4\n    , filepath              >=1.3.0.0 && <1.5\n    , parsec                >=3.1.11  && <3.2\n    , text                  >=1.2.3.0 && <1.3 || >=2.0 && <2.1\n    , transformers          >=0.3.0.0 && <0.7\n    , unordered-containers  >=0.2.5   && <0.3\n    , vector                >=0.11    && <0.14\n\n  if impl(ghc <=7.6)\n    build-depends: ghc-prim\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18 && <0.21\n\n  exposed-modules:\n    Text.Microstache\n    Text.Microstache.Compile\n    Text.Microstache.Parser\n    Text.Microstache.Render\n    Text.Microstache.Type\n\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  default-language: Haskell2010\n\ntest-suite spec\n  main-is:          Spec.hs\n  hs-source-dirs:   tests tasty-as-hspec\n  type:             exitcode-stdio-1.0\n  build-depends:\n      aeson\n    , base\n    , containers\n    , microstache\n    , parsec\n    , text\n\n  -- tasty-as-hspec\n  build-depends:\n      base-orphans  >=0.8.7    && <0.9\n    , tasty         >=1.4.0.1  && <1.5\n    , tasty-hunit   >=0.10.0.3 && <0.11\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups\n\n  other-modules:\n    Test.Hspec\n    Text.Microstache.ParserSpec\n    Text.Microstache.RenderSpec\n    Text.Microstache.TypeSpec\n\n  default-language: Haskell2010\n\ntest-suite mustache-spec\n  main-is:          Spec.hs\n  hs-source-dirs:   mustache-spec tasty-as-hspec\n  type:             exitcode-stdio-1.0\n  build-depends:\n      aeson\n    , base\n    , bytestring\n    , containers\n    , microstache\n    , parsec\n    , text\n\n  -- tasty-as-hspec\n  build-depends:\n      base-orphans  >=0.8.7    && <0.9\n    , tasty         >=1.4.0.1  && <1.5\n    , tasty-hunit   >=0.10.0.3 && <0.11\n\n  other-modules:    Test.Hspec\n  default-language: Haskell2010\n";
    }