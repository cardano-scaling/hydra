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
    flags = { dev = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "modern-uri"; version = "0.3.4.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mark Karpov <markkarpov92@gmail.com>";
      author = "Mark Karpov <markkarpov92@gmail.com>";
      homepage = "https://github.com/mrkkrp/modern-uri";
      url = "";
      synopsis = "Modern library for working with URIs";
      description = "Modern library for working with URIs.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-megaparsec" or (errorHandler.buildDepError "hspec-megaparsec"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."modern-uri" or (errorHandler.buildDepError "modern-uri"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-speed" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."modern-uri" or (errorHandler.buildDepError "modern-uri"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "bench-memory" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."modern-uri" or (errorHandler.buildDepError "modern-uri"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."weigh" or (errorHandler.buildDepError "weigh"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/modern-uri-0.3.4.4.tar.gz";
      sha256 = "5fe3529a8e0ae5cdd18e2d30137f21c0942e0ef2d2e7f694542b9e358f77cea5";
      });
    }) // {
    package-description-override = "cabal-version:   2.4\r\nname:            modern-uri\r\nversion:         0.3.4.4\r\nx-revision: 2\r\nlicense:         BSD-3-Clause\r\nlicense-file:    LICENSE.md\r\nmaintainer:      Mark Karpov <markkarpov92@gmail.com>\r\nauthor:          Mark Karpov <markkarpov92@gmail.com>\r\ntested-with:     ghc ==8.10.7 ghc ==9.0.2 ghc ==9.2.1\r\nhomepage:        https://github.com/mrkkrp/modern-uri\r\nbug-reports:     https://github.com/mrkkrp/modern-uri/issues\r\nsynopsis:        Modern library for working with URIs\r\ndescription:     Modern library for working with URIs.\r\ncategory:        Text\r\nbuild-type:      Simple\r\nextra-doc-files:\r\n    CHANGELOG.md\r\n    README.md\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/mrkkrp/modern-uri.git\r\n\r\nflag dev\r\n    description: Turn on development settings.\r\n    default:     False\r\n    manual:      True\r\n\r\nlibrary\r\n    exposed-modules:\r\n        Text.URI\r\n        Text.URI.Lens\r\n        Text.URI.QQ\r\n\r\n    other-modules:\r\n        Text.URI.Parser.ByteString\r\n        Text.URI.Parser.Text\r\n        Text.URI.Parser.Text.Utils\r\n        Text.URI.Render\r\n        Text.URI.Types\r\n\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        QuickCheck >=2.4 && <3.0,\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        containers >=0.5 && <0.7,\r\n        contravariant >=1.3 && <2.0,\r\n        deepseq >=1.3 && <1.5,\r\n        exceptions >=0.6 && <0.11,\r\n        megaparsec >=7.0 && <10.0,\r\n        mtl >=2.0 && <3.0,\r\n        profunctors >=5.2.1 && <6.0,\r\n        reflection >=2.0 && <3.0,\r\n        tagged >=0.8 && <0.9,\r\n        template-haskell >=2.10 && <2.20,\r\n        text >=0.2 && <2.1\r\n\r\n    if flag(dev)\r\n        ghc-options: -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n\r\n    if flag(dev)\r\n        ghc-options:\r\n            -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns\r\n            -Wnoncanonical-monad-instances\r\n\r\ntest-suite tests\r\n    type:               exitcode-stdio-1.0\r\n    main-is:            Spec.hs\r\n    build-tool-depends: hspec-discover:hspec-discover >=2.0 && <3.0\r\n    hs-source-dirs:     tests\r\n    other-modules:\r\n        Text.QQSpec\r\n        Text.URISpec\r\n\r\n    default-language:   Haskell2010\r\n    build-depends:\r\n        QuickCheck >=2.4 && <3.0,\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        hspec >=2.0 && <3.0,\r\n        hspec-megaparsec >=2.0 && <3.0,\r\n        megaparsec >=8.0 && <10.0,\r\n        modern-uri,\r\n        text >=0.2 && <2.1\r\n\r\n    if flag(dev)\r\n        ghc-options: -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n\r\nbenchmark bench-speed\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   bench/speed\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        criterion >=0.6.2.1 && <1.7,\r\n        megaparsec >=8.0 && <10.0,\r\n        modern-uri,\r\n        text >=0.2 && <2.1\r\n\r\n    if flag(dev)\r\n        ghc-options: -O2 -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n\r\nbenchmark bench-memory\r\n    type:             exitcode-stdio-1.0\r\n    main-is:          Main.hs\r\n    hs-source-dirs:   bench/memory\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base >=4.13 && <5.0,\r\n        bytestring >=0.2 && <0.12,\r\n        deepseq >=1.3 && <1.5,\r\n        megaparsec >=8.0 && <10.0,\r\n        modern-uri,\r\n        text >=0.2 && <2.1,\r\n        weigh >=0.0.4\r\n\r\n    if flag(dev)\r\n        ghc-options: -O2 -Wall -Werror\r\n\r\n    else\r\n        ghc-options: -O2 -Wall\r\n";
    }